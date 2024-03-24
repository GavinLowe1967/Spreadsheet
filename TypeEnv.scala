package spreadsheet

import scala.collection.immutable.{Map,HashMap}

  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)

object TypeEnv{
  /** A mapping from names in the script to their types. */
  private type NameMap = HashMap[Name, TypeT]

  /** A mapping from type identifiers to MemberOf(ts) constraints. */
  private type Constraints = HashMap[TypeID, StoredTypeConstraint]

  /** A frame, corresponding to a particular nesting of scopes. */
  private class Frame{
    /* When a name `n` gets overwritten by a new identifier with the same name, 
     * it gets replaced in the NameMap by `getNewName(n)`.  The old value is
     * restored when the current scope is exited, using `getOldName`. */  

    private var newNameCounter = 0

    /** A new name with which n is replaced.  Guaranteed to be different from any
      * other name being used. */
    private def getNewName(n: Name) = {
      val c = newNameCounter; newNameCounter += 1; n+"$"+c
    }

    /** The old name for nn.  Spec: getOldName(getNewName(n)) = n. */
    private def getOldName(nn: Name) = nn.takeWhile(_ != '$')

    /** Names that have been declared in the current scope. */
    var newNames = List[Name]()

    /** The temporary names that are used in place of names that have been
      * overwritten in the current scope. */
    var tempNames = List[Name]()

    /** Record that name is being overwritten in the current scope.  Return the
      * new name to use in place of name. */
    def storeOldName(name: Name): Name = {
      val newName = getNewName(name); tempNames ::= newName; newName
    }

    /** Record that name is a new name in the current scope. */
    def storeNewName(name: Name) = newNames ::= name

    /** Update nameMap corresponding to leaving the current scope.  Replace each
      * temporary name with the corresponding new name, and remove the new
      * names from this scope. */
    def updateAtEndOfScope(nameMap: NameMap): NameMap = {
      nameMap -- newNames ++ 
      (for(name <- tempNames) yield getOldName(name) -> nameMap(name)) -- 
      tempNames
    }
  }

  /** An initial TypeEnv. */
  def apply() =
    new TypeEnv(new NameMap, new Constraints, new Frame, List[Frame]())
} 

import TypeEnv._

/** A type environment.
  * @param nameMap A mapping from names in the script to their types.
  * @param constraints A mapping from type identifiers to constraints.
  * @param frame The Frame corresponding to the current scope. 
  * @param stack The frames for outer scopes. 
  * Inv: constraints includes a mapping tid -> MemberOf(ts) for each
  * TypeVar(tid) used in nameMap. 
  * Note: type environments are treated immutably. */
class TypeEnv(
  private val nameMap: NameMap, 
  private val constraints: Constraints,
  private val frame: Frame,
  private val stack: List[Frame]
){
  /** The type associated with name. */
  def apply(name: Name) : TypeT = nameMap(name)

  /** Optionally, the type associated with name. */
  def get(name: Name): Option[TypeT] = nameMap.get(name)

  /** The constraint associated with tid. */
  def apply(tid: TypeID) : StoredTypeConstraint = constraints(tid)

  /** The TypeEnv formed by adding the mapping name -> t. */
  def + (name: Name, t: TypeT) : TypeEnv = {
    val newNameMap = nameMap.get(name) match{
      case Some(tt) => // replace name by newName
        val newName = frame.storeOldName(name)
        nameMap ++ List(newName -> tt, name -> t)
      case None => nameMap + (name -> t)
    }
    frame.storeNewName(name)
    new TypeEnv(newNameMap, constraints, frame, stack)
  }

  /** The TypeEnv formed by adding each name -> t for (name,t) in pairs. */
  def ++ (pairs: List[(Name, TypeT)]) : TypeEnv = {
    var updates = pairs // the updates to make to nameMap
    for((name, t) <- pairs){
      nameMap.get(name) match{
        case Some(tt) => 
          val newName = frame.storeOldName(name); updates ::= (newName -> tt)
        case None => {}
      }
      frame.storeNewName(name)
    }
    new TypeEnv(nameMap ++ updates, constraints, frame, stack)
  }

  /** The TypeEnv formed by adding  the constraint typeID -> tc. */
  def + (typeID: TypeID, tc: StoredTypeConstraint) : TypeEnv = 
    new TypeEnv(nameMap, constraints + (typeID -> tc), frame, stack)

  /** The TypeEnv formed from this by replacing TypeVar(tId) with t. */
  def replace(tId: TypeID, t: TypeT): TypeEnv = {
    val newNameMap =
      nameMap.map{ case (n,t1) => (n, Substitution.reMap(tId, t, t1)) }
    new TypeEnv(newNameMap, constraints - tId, frame, stack)
// TODO: the update needs to be propagated to the CellExpr that generated this
// type identifier. 
  }

  /** Record that a new scope is being entered. */
  def newScope = 
    new TypeEnv(nameMap, constraints, new Frame, frame :: stack)

  /** End the current scope.  Return the type environment to use in the outer
    * scope. */
  def endScope : TypeEnv = {
    require(stack.nonEmpty)
    val newNameMap = frame.updateAtEndOfScope(nameMap)
    new TypeEnv(newNameMap, constraints, stack.head, stack.tail)
  }

  def showType(t: TypeT): String = t match{
    case TypeVar(tId) => constraints(tId).asString
    case _ => t.asString
  }

  override def toString = s"TypeEnv($nameMap, $constraints)"
}
