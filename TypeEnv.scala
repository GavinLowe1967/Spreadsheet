package spreadsheet

import scala.collection.immutable.{Map,HashMap}

  import TypeVar.TypeID // Type variables (Ints)
  import NameExp.Name // Names of identifiers (Strings)

object TypeEnv{
  /** A mapping from names in the script to their types. */
  private type NameMap = HashMap[Name, TypeT]

  /** A mapping from type identifiers to MemberOf(ts) constraints. */
  private type Constraints = HashMap[TypeID, StoredTypeConstraint]

  /** A mapping giving the CellExprs whose type matches a particular TypeVar. */
  private type CellReadMap = HashMap[TypeID, List[CellExp]]

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
    new TypeEnv(
      new NameMap, new Constraints, new CellReadMap,  new Frame, List[Frame]())
} 

// ==================================================================

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
  private val cellReadMap: CellReadMap,
  private val frame: Frame,
  private val stack: List[Frame]
){
  /** The type associated with name. */
  def apply(name: Name) : TypeT = nameMap(name)

  /** Optionally, the type associated with name. */
  def get(name: Name): Option[TypeT] = nameMap.get(name)

  /** The constraint associated with tid. */
  def apply(tid: TypeID) : StoredTypeConstraint = constraints(tid) match{
    case SingletonTypeConstraint(TypeVar(tid1)) =>
      println(s"TypeEnv.apply: $tid -> $tid1"); apply(tid1)
    case c => c 
  }
// IMPROVE: comment

  /** The TypeEnv formed by adding the mapping name -> t. */
  def + (name: Name, t: TypeT) : TypeEnv = {
    val newNameMap = nameMap.get(name) match{
      case Some(tt) => // replace name by newName
        val newName = frame.storeOldName(name)
        nameMap ++ List(newName -> tt, name -> t)
      case None => nameMap + (name -> t)
    }
    frame.storeNewName(name)
    new TypeEnv(newNameMap, constraints, cellReadMap, frame, stack)
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
    new TypeEnv(nameMap ++ updates, constraints, cellReadMap, frame, stack)
  }

  /** The TypeEnv formed by adding  the constraint typeID -> tc. */
  def + (typeID: TypeID, tc: StoredTypeConstraint) : TypeEnv = 
    new TypeEnv(nameMap, constraints + (typeID -> tc), cellReadMap, frame, stack)

  def addCellConstraint(typeId: TypeID, cell: CellExp): TypeEnv = {
    assert(!cellReadMap.contains(typeId))
    val newConstraints = constraints + (typeId -> MemberOf(CellTypes))
    val newCellReadMap = cellReadMap + (typeId -> List(cell))
    new TypeEnv(nameMap, newConstraints, newCellReadMap, frame, stack)
  }

  val CellTypes = List(IntType, FloatType, StringType, BoolType) // FIXME: move - repeated in TypeChecker

  /** The TypeEnv formed from this by replacing TypeVar(tId) with t.
    * @param evalTime Is this at evaluation time (as opposed to during
    * typechecking)?  If so, do not propagate updates to cell expressions.  */
  def replace(tId: TypeID, t: TypeT, evalTime: Boolean = false): TypeEnv = {
    // Update relevant cell expressions with type t.  Build new CellReadMap
    val newCellReadMap = 
      if(evalTime) cellReadMap
      else cellReadMap.get(tId) match {
        case Some(cellEs) =>
          // println(s"TypeEnv.replace: $cells -> $t")
          for(cellE <- cellEs) cellE.setType(t)
          t match{
            case TypeVar(tId1) =>
              // Add cells1 to celllReadMap(tId1)
              cellReadMap.get(tId1) match{
                case Some(cellEs1) => cellReadMap + (tId1 -> (cellEs1++cellEs))
                case None => cellReadMap + (tId1 -> cellEs)
              }
            case _ => cellReadMap - tId
          }
        case None => cellReadMap
      }                //  end of definition of newCellReadMap
    // Note: we don't propagate updates to cell expressions during evaluation,
    // since if there are subsequent changes to the spreadsheet, the updates
    // to cell expressions would be invalid.
    val newNameMap =
      nameMap.map{ case (n,t1) => (n, Substitution.reMap(tId, t, t1)) }
    val newConstraints = 
      if(evalTime) constraints + (tId -> SingletonTypeConstraint(t))
      else constraints
    new TypeEnv(newNameMap, newConstraints, newCellReadMap, frame, stack)
    // Note: we keep tId in constraints, because it might be associated
    // with a CellExpr.  We could propagate this update to the CellExpr.
  }

  /** Record that a new scope is being entered. */
  def newScope = 
    new TypeEnv(nameMap, constraints, cellReadMap, new Frame, frame :: stack)

  /** End the current scope.  Return the type environment to use in the outer
    * scope. */
  def endScope : TypeEnv = {
    require(stack.nonEmpty)
    val newNameMap = frame.updateAtEndOfScope(nameMap)
    new TypeEnv(newNameMap, constraints, cellReadMap, stack.head, stack.tail)
  }

  def showType(t: TypeT): String = t match{
    case TypeVar(tId) => apply(tId).asString
    case _ => t.asString
  }

  override def toString = s"TypeEnv($nameMap, $constraints)"
}
