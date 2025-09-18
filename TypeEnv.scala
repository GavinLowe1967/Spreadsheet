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
  private val stack: List[Frame]
) extends EvaluationTypeEnv(constraints, typeParamMap){

  /** Make a new TypeEnv, using the parameters of this where not specified. */
  private def make(
    nameMap: NameMap = nameMap, 
    constraints: Constraints = constraints,
    typeParamMap: TypeParamMap = typeParamMap,
    stack: List[Frame] = stack
  ) = new TypeEnv(nameMap, constraints, typeParamMap, stack)

  // ========= NameMap functions

  /** The type associated with name. */
  def apply(name: Name): TypeT = nameMap(name)

  /** Optionally, the type associated with name. */
  def get(name: Name): Option[TypeT] = nameMap.get(name)

  /** The TypeEnv formed by adding the mapping name -> t. */
  def + (name: Name, t: TypeT) : TypeEnv = make(nameMap + (name -> t))

  /** The TypeEnv formed by adding each name -> t for (name,t) in pairs. */
  def ++ (pairs: List[(Name, TypeT)]) : TypeEnv = make(nameMap ++ pairs)

  /** Remove name from the type environment. */
  def - (name: Name): TypeEnv = make(nameMap - name)

  /** Replace tId by t in nameMap. */
  private def subInNameMap(nameMap: NameMap, tId: TypeID, t: TypeT): NameMap =
    nameMap.map{ case (n,t1) => (n, Substitution.reMap(tId, t, t1)) }

  // ========= Constraints functions

  /** The TypeEnv formed by adding  the constraint typeID -> tc. */
  def + (typeID: TypeID, tc: TypeConstraint): TypeEnv = 
    make(constraints = constraints + (typeID -> tc))

  /** The TypeEnv formed by adding the constraint typeID -> tc for each (typeID,
    * tc) in pairs. */
  def addConstraints(pairs: List[(TypeID, TypeConstraint)]): TypeEnv = 
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

  def + (n: TypeParam.TypeParamName, c: TypeParamConstraint): TypeEnv = 
    make(typeParamMap = typeParamMap + (n -> c))
    //addTypeParamConstraint(n, c)

  /** Add type parameters and associated constraints. */
  def addTypeParams(pairs: List[FunctionType.TypeParameter]): TypeEnv = 
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
  def newScope: TypeEnv = make(stack = new Frame(nameMap,typeParamMap) :: stack)

  /** End the current scope.  Return the type environment to use in the outer
    * scope. */
  def endScope: TypeEnv = {
    require(stack.nonEmpty)
    val Frame(newNameMap, newTypeParamMap) = stack.head
    make(nameMap = newNameMap, typeParamMap = newTypeParamMap, 
      stack = stack.tail)
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
  private type NameMap = HashMap[Name, TypeT] // immutable 

  /** A stack frame, storing information about the type environment for the
    * outer scope. */
  case class Frame(nameMap: NameMap, typeParamMap: TypeParamMap)

  /** An initial TypeEnv. */
  def apply() = {
    val nameMap = new NameMap ++ BuiltInFunctions.builtInTypes
    new TypeEnv(nameMap, new Constraints, new TypeParamMap, List[Frame]())
  }

  private val builtInNames = BuiltInFunctions.builtInTypes.map(_._1)
} 
