package spreadsheet

import scala.collection.immutable.{Map,HashMap}

import TypeVar.TypeID // Type variables (Ints)
import TypeParam.TypeParamName // Names of type parameters (Strings)
import NameExp.Name // Names of identifiers (Strings)
import EvaluationTypeEnv._
import TypeEnv._

/** A type environment.
  * @param nameMap A mapping from names in the script to their types.
  * @param constraints A mapping from type identifiers to constraints.
  * @param typeParamMap A mapping from TypeParamName to TypeConstraint
  * @param frame The Frame corresponding to the current scope. 
  * @param stack The frames for outer scopes. 
  * Note: type environments are treated immutably. */
class TypeEnv(
  private val nameMap: NameMap, 
  private val constraints: Constraints,
  private val typeParamMap: TypeParamMap,
  private val frame: Frame,
  private val stack: List[Frame]
) extends EvaluationTypeEnv(constraints, typeParamMap){

  /** Make a new TypeEnv, using the parameters of this where not specified. */
  private def make(
    nameMap: NameMap = nameMap, 
    constraints: Constraints = constraints,
    typeParamMap: TypeParamMap = typeParamMap,
    frame: Frame = frame, stack: List[Frame] = stack
  ) = new TypeEnv(nameMap, constraints, typeParamMap, frame, stack)

  // ========= NameMap functions

  /** The type associated with name. */
  def apply(name: Name): TypeT = nameMap(name)

  /** Optionally, the type associated with name. */
  def get(name: Name): Option[TypeT] = nameMap.get(name)

  /** The TypeEnv formed by adding the mapping name -> t. */
  def + (name: Name, t: TypeT) : TypeEnv = {
    val newNameMap = nameMap.get(name) match{
      case Some(tt) => // replace name by newName
        val newName = frame.storeOldName(name)
        nameMap ++ List(newName -> tt, name -> t)
      case None => nameMap + (name -> t)
    }
    frame.storeNewName(name)
    make(newNameMap)
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
    make(nameMap ++ updates)
  }

  /** Remove name from the type environment. */
  def - (name: Name): TypeEnv = make(nameMap - name)

  /** Replace tId by t in nameMap. */
  private def subInNameMap(nameMap: NameMap, tId: TypeID, t: TypeT) : NameMap =
    nameMap.map{ case (n,t1) => (n, Substitution.reMap(tId, t, t1)) }

  // ========= Constraints functions

  /** The TypeEnv formed by adding  the constraint typeID -> tc. */
  def addTypeVarConstraint(typeID: TypeID, tc: TypeConstraint) : TypeEnv = 
    make(constraints = constraints + (typeID -> tc))

  /** The TypeEnv formed by adding  the constraint typeID -> tc. */
  def + (typeID: TypeID, tc: TypeConstraint) : TypeEnv = 
    addTypeVarConstraint(typeID, tc)

  /** The TypeEnv formed by adding the constraint typeID -> tc for each (typeID,
    * tc) in pairs. */
  def addConstraints(pairs: List[(TypeID, TypeConstraint)]) : TypeEnv = 
    make(constraints = constraints ++ pairs)

  /** Is t an equality type? */
  def isEqType(t: TypeT): Boolean = t match{
    case TypeVar(tv) => true // ???, FIXME
    case TypeParam(name) =>
      constraintForTypeParam(name).implies(EqTypeConstraint)
    case _: EqType => true
    case ListType(underlying) => isEqType(underlying)
    case FunctionType(_,_,_) => false
  }

  // ========= TypeParamMap functions

  def addTypeParamConstraint(n: TypeParam.TypeParamName, c: TypeParamConstraint)
      : TypeEnv =
    make(typeParamMap = typeParamMap + (n -> c))

  def + (n: TypeParam.TypeParamName, c: TypeParamConstraint): TypeEnv = 
    addTypeParamConstraint(n, c)

  /** Add type parameters and associated constraints. */
  def addTypeParams(pairs: List[FunctionType.TypeParameter]) : TypeEnv = 
    make(typeParamMap = typeParamMap ++ pairs)

  /** Is n a defined type parameter? */
  def hasTypeParam(n: TypeParamName) = typeParamMap.contains(n)

  // ========= update functions

  /** The TypeEnv formed from this by replacing TypeVar(tId) with t, as used
    * during type checking.  Updates are propogated to cell expressions.  */
  def replace(tId: TypeID, t: TypeT): TypeEnv = {
    val newNameMap = subInNameMap(nameMap, tId, t)
    // Note: we update the mapping for tId to deal with cases like 
    // "def f[A](x: A, y: A): A = x; val y = f(3, true)"
    val newConstraints = constraints + (tId -> SingletonTypeConstraint(t))
    make(nameMap = newNameMap, constraints = newConstraints)
  }

  // ========= Scoping functions

  /** Record that a new scope is being entered. */
  def newScope: TypeEnv = {
    frame.storeTParamMap(typeParamMap)
    make(frame = new Frame, stack = frame::stack)
  }

  /** End the current scope.  Return the type environment to use in the outer
    * scope. */
  def endScope: TypeEnv = {
    require(stack.nonEmpty)
    val newNameMap = frame.updateAtEndOfScope(nameMap)
    val newFrame = stack.head; val newTParamMap = newFrame.getTParamMap
    make(nameMap = newNameMap, typeParamMap = newTParamMap, 
      frame = newFrame, stack = stack.tail)
  }

  // ========= For evaluation

  /** Get an EvaluationTypeEnv including the relevant components of this. */
  def getEvaluationTypeEnv = new EvaluationTypeEnv(constraints, typeParamMap)

  // ========= Generic helper functions

  private def showNameMap = (
    for((n,v) <- nameMap; if ! builtInNames.contains(n)) yield s"$n -> $v"
  ).mkString("\n{ ", ",\n  ", " }")

  override def toString = 
    s"TypeEnv(nameMap = $showNameMap,\nconstraints = $constraints,\n"+
      s"typeParamMap = $typeParamMap)"
}

// ==================================================================

/** Companion object for TypeEnv. */
object TypeEnv{
  /** A mapping from names in the script to their types. */
  private type NameMap = HashMap[Name, TypeT]

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
    private var newNames = List[Name]()

    /** The temporary names that are used in place of names that have been
      * overwritten in the current scope. */
    private var tempNames = List[Name]()

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

    /** The TypeParamMap from the previous scope. */
    private var oldTParamMap : TypeParamMap = null

    def storeTParamMap(tParamMap: TypeParamMap) = oldTParamMap = tParamMap

    def getTParamMap = { assert(oldTParamMap != null); oldTParamMap }

  }                   // End of Frame

  /** An initial TypeEnv. */
  def apply() = {
    val nameMap = new NameMap ++ BuiltInFunctions.builtInTypes
    new TypeEnv(
      nameMap, new Constraints, new TypeParamMap, new Frame, List[Frame]())
  }

  private val builtInNames = BuiltInFunctions.builtInTypes.map(_._1)
} 
