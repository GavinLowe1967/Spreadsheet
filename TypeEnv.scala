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
  }                   // End of Frame

  /** An initial TypeEnv. */
  def apply() = {
    val nameMap = 
      new NameMap + ("head" -> PolymorphicFunction.head) + 
        ("tail" -> PolymorphicFunction.tail)
// FIXME: add built in functions
    new TypeEnv(
      nameMap, new Constraints, new CellReadMap, new Frame, List[Frame]())

  }
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
) extends TypeEnv0{
  /** The type associated with name. */
  def apply(name: Name) : TypeT = nameMap(name)

  /** Optionally, the type associated with name. */
  def get(name: Name): Option[TypeT] = nameMap.get(name)

  /** The constraint associated with tid. */
  def apply(tid: TypeID) : StoredTypeConstraint = constraints(tid) match{
    case SingletonTypeConstraint(TypeVar(tid1)) =>       // IMPROVE, unreachable?
      assert(false) 
      println(s"TypeEnv.apply: $tid -> $tid1"); apply(tid1)
    case c => c 
  }

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

  /** Remove name from the type environment. */
  def - (name: Name): TypeEnv = 
    new TypeEnv(nameMap - name, constraints, cellReadMap, frame, stack)

  /** The TypeEnv formed by adding the constraint typeID -> tc for each (typeID,
    * tc) in pairs. */
  def addConstraints(pairs: List[(TypeID, StoredTypeConstraint)]) : TypeEnv = {
    new TypeEnv(nameMap, constraints ++ pairs, cellReadMap, frame, stack)
  }

  /** Add a constraint that typeId is a Cell type corresponding to `cell`. */
  def addCellConstraint(typeId: TypeID, cell: CellExp): TypeEnv = {
    assert(!cellReadMap.contains(typeId))
    val newConstraints = constraints + (typeId -> MemberOf(TypeT.CellTypes))
    val newCellReadMap = cellReadMap + (typeId -> List(cell))
    new TypeEnv(nameMap, newConstraints, newCellReadMap, frame, stack)
  }

  /** Replace tId by t in nameMap. */
  private def subInNameMap(nameMap: NameMap, tId: TypeID, t: TypeT) : NameMap =
    nameMap.map{ case (n,t1) => (n, Substitution.reMap(tId, t, t1)) }

  /** The TypeEnv formed from this by replacing TypeVar(tId) with t, as used
    * during type checking.  Updates are propogated to cell expressions.  */
  def replace(tId: TypeID, t: TypeT): TypeEnv = {
    // Update relevant cell expressions with type t.  Build new CellReadMap
    val newCellReadMap = cellReadMap.get(tId) match {
        case Some(cellEs) =>
          // println(s"TypeEnv.replace: $cells -> $t")
          for(cellE <- cellEs) cellE.setType(t)
          t match{
            case TypeVar(tId1) =>       // Add cellEs to celllReadMap(tId1)
              val newCellEs = cellReadMap.get(tId1) match{
                case Some(cellEs1) => cellEs1++cellEs; case None => cellEs
              }
              cellReadMap - tId + (tId1 -> newCellEs)

            case _ => cellReadMap - tId
          } // end of "t match"

        case None => cellReadMap
      }                //  end of definition of newCellReadMap
    val newNameMap = subInNameMap(nameMap, tId, t)
    new TypeEnv(newNameMap, constraints, newCellReadMap, frame, stack)
    //new TypeEnv(newNameMap, constraints - tId, newCellReadMap, frame, stack)
  }

  /** The TypeEnv formed from this by replacing TypeVar(tId) with t, as used at
    * evaluation time: updates are not propagated to cell expressions. */
  def replaceEvalTime(tId: TypeID, t: TypeT): TypeEnv = {
    // Note: we don't propagate updates to cell expressions during evaluation,
    // since if there are subsequent changes to the spreadsheet, the updates
    // to cell expressions would be invalid.  We just update constraints.
    val newNameMap = subInNameMap(nameMap, tId, t)
    val newConstraints = constraints + (tId -> SingletonTypeConstraint(t))
    new TypeEnv(newNameMap, newConstraints, cellReadMap, frame, stack)
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
