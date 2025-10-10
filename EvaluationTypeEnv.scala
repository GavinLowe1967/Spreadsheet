package spreadsheet

import scala.collection.immutable.{Map,HashMap}

import TypeVar.TypeID // Type variables (Ints)
import EvaluationTypeEnv._
import TypeParam.TypeParamName // Names of type parameters (Strings)

/** A type environment for use during evaluation. 
  * @param constraints a mapping giving constraints on type variables.
  * @param typeParamMap a mapping giving constraints on type parameters.  */
class EvaluationTypeEnv(
  private val constraints: Constraints, // = HashMap[TypeID, TypeConstraint]
  private val typeParamMap: TypeParamMap 
                              // = HashMap[TypeParamName, TypeParamConstraint]
) extends TypeEnv0{

  // ========= Constraints functions

  /** The constraint associated with tid. */
  def apply(tid: TypeID) : TypeConstraint = constraints(tid) match{
    case SingletonTypeConstraint(TypeVar(tid1)) =>  apply(tid1) 
      // Arises with, e.g., "def f[A](x: A, y: A): A = x; val y = f(3, 4)"
    case c => c 
  }

  // ========= TypeParamMap functions

  /** The constraint associated with TypeParam(n). */
  def constraintForTypeParam(n: TypeParamName): TypeParamConstraint =
    typeParamMap(n)

  // ========= Helper function

  /** A string representing type t, with relevant constraints. */
  def showType(t: TypeT): String = t match{
    case TypeVar(tId) => apply(tId).asStringE
    case TypeParam(tp) => typeParamMap(tp) match{
      case AnyTypeConstraint => tp; case c => s"$tp <: "+c.asString
    }
    case ListType(underlying) => s"List[${showType(underlying)}]"
    case _ => t.asString
  }
}

// ==================================================================

object EvaluationTypeEnv{
  /** A mapping from type identifiers to constraints. */
  type Constraints = HashMap[TypeID, TypeConstraint]

  /** Mapping giving the type constraints on type parameters currently in
    * scope. */
  type TypeParamMap = HashMap[TypeParamName, TypeParamConstraint]
}
