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
  * @param untypedCells The UntypedCellExps for which we are seeking the type.
  * param cellTypeVarTypes map showing types for some UntypecCellExps.
  * @param stack The frames for outer scopes. 
  * Note: type environments are treated immutably. */
class TypeEnv(
  private val nameMap: NameMap, 
  private val constraints: Constraints, // = HashMap[TypeID, TypeConstraint]
  private val typeParamMap: TypeParamMap, // HashMap[TypeParamName, TypeParamConstraint]
  private val untypedCells: List[UntypedCellExp],
  private val cellTypeVarTypes: Map[CellTypeVar, CellType], 
  private val stack: List[Frame]
) extends EvaluationTypeEnv(constraints, typeParamMap){

  /** Make a new TypeEnv, using the parameters of this where not specified. */
  private def make(
    nameMap: NameMap = nameMap, 
    constraints: Constraints = constraints,
    typeParamMap: TypeParamMap = typeParamMap,
    untypedCells: List[UntypedCellExp] = untypedCells,
    cellTypeVarTypes: Map[CellTypeVar, CellType] = cellTypeVarTypes,
    stack: List[Frame] = stack
  ) = new TypeEnv(
    nameMap, constraints, typeParamMap, untypedCells, cellTypeVarTypes, stack)

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

/*
  /** Is t an equality type? */
  def isEqType(t: TypeT): Boolean = t match{
    case TypeVar(tv) => true // ???, FIXME.  Arises from "[] == [3]"
    case TypeParam(name) =>
      constraintForTypeParam(name).implies(EqTypeConstraint)
    case _: EqType => true
    case _: CellTypeVar => ??? // true
    case ListType(underlying) => isEqType(underlying)
    case FunctionType(_,_,_) => false
  }
 */
  /** Try to extend this so that t is an equality type. */
  def mkEqType(t: TypeT): Reply[TypeEnv] = {
    def fail = FailureR(s"Expected equality type, found ${t.asString}")
    t match{
      case TypeVar(tv) => 
        apply(tv) match{
          case EqTypeConstraint => Ok(this)
          case AnyTypeConstraint => Ok(this+(tv,EqTypeConstraint))
          case SingletonTypeConstraint(t1) => println(t1); ??? 
            // I think mkEqType(t1), but I can't find a test for this.
        }
      case TypeParam(name) =>
        if(constraintForTypeParam(name).implies(EqTypeConstraint)) Ok(this)
        else fail
      case _: EqType => Ok(this)
      case _: CellTypeVar => sys.error(s"Shouldn't happen: mkEqType($t)")
      case ListType(underlying) => mkEqType(underlying)
      case ft: FunctionType => fail
    }
  }

  // ========= TypeParamMap functions

  /** Extend the TypeParamMap with n -> c. */
  def + (n: TypeParam.TypeParamName, c: TypeParamConstraint): TypeEnv = 
    make(typeParamMap = typeParamMap + (n -> c))
    //addTypeParamConstraint(n, c)

  /** Add type parameters and associated constraints. */
  def addTypeParams(pairs: List[FunctionType.TypeParameter]): TypeEnv = 
    make(typeParamMap = typeParamMap ++ pairs)

  /** Is n a defined type parameter? */
  def hasTypeParam(n: TypeParamName) = typeParamMap.contains(n)

  // ========= UntypedCellExp functions

  /** Add cell to the untyped cells. */
  def + (cell: UntypedCellExp): TypeEnv = 
    make(untypedCells = cell::untypedCells)

  /** Add tv -> ct to cellTypeVarTypes. */
  def + (tv: CellTypeVar, ct: CellType) = {
    assert(!cellTypeVarTypes.contains(tv))
    make(cellTypeVarTypes = cellTypeVarTypes + (tv -> ct))
  }

  /** Where the type of an UntypedCellExp has been found, store that in the
    * UntypecCellExp; return those for which no type has been found. */
  def getUntypedCells: List[UntypedCellExp] = {
    var result = List[UntypedCellExp]()
    for(cell <- untypedCells) cellTypeVarTypes.get(cell.getTypeVar) match{
      case Some(t) => cell.setType(t); 
      case None => result ::= cell
    }
    result
  }

  /** Remove information about untyped cells from this.  Pre: getUntypedCells
    * would return List(). */
  def removeUntypedCells: TypeEnv =
    make(untypedCells = List(), cellTypeVarTypes = Map())

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
  // Note: no need to store information about untypedCells here, because that
  // information should be empty when we leave a scope.  We should preserve
  // constraints when we leave a scope, though: we might have learnt something
  // new from the inner scope.

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
      s"typeParamMap = $typeParamMap,\nuntypedCells = $untypedCells,\n"+
      s"cellTypeVarTypes = $cellTypeVarTypes)"
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
    new TypeEnv(nameMap, new Constraints, new TypeParamMap, 
      List[UntypedCellExp](), Map[CellTypeVar, CellType](), List[Frame]())
  }

  private val builtInNames = BuiltInFunctions.builtInTypes.map(_._1)
} 
