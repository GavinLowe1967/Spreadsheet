package spreadsheet

import scala.collection.immutable.{Map,HashMap}

import TypeVar.TypeID // Type variables (Ints)
import EvaluationTypeEnv._
import TypeParam.TypeParamName // Names of type parameters (Strings)

/** A type environment for use during evaluation. */
class EvaluationTypeEnv(
  private val constraints: Constraints,
  private val typeParamMap: TypeParamMap
) extends TypeEnv0{

  /** The constraint associated with tid. */
  def apply(tid: TypeID) : StoredTypeConstraint = constraints(tid) match{
    case SingletonTypeConstraint(TypeVar(tid1)) =>       // IMPROVE, unreachable?
      assert(false) 
      println(s"TypeEnv.apply: $tid -> $tid1"); apply(tid1)
    case c => c 
  }

  /**The constraint associated with TypeParam(n). */
  def constraintForTypeParam(n: TypeParamName): TypeParamConstraint =
    typeParamMap(n)

  /** The EvaluationTypeEnv formed from this by replacing TypeVar(tId) with t.
    */
  def replaceEvalTime(tId: TypeID, t: TypeT): EvaluationTypeEnv = {
    // Note: we don't propagate updates to cell expressions during evaluation,
    // since if there are subsequent changes to the spreadsheet, the updates
    // to cell expressions would be invalid.  We just update constraints.
    val newConstraints = constraints + (tId -> SingletonTypeConstraint(t))
    new EvaluationTypeEnv(newConstraints, typeParamMap)
  }

  /** A string representing type t, with relevant constraints. */
  def showType(t: TypeT): String = t match{
    case TypeVar(tId) => apply(tId).asStringE
    case TypeParam(tp) => typeParamMap(tp) match{
      case AnyTypeConstraint => tp; case c => s"$tp <: "+c.asString
    }
    case _ => t.asString
  }
}

// ==================================================================

object EvaluationTypeEnv{
  /** A mapping from type identifiers to MemberOf(ts) constraints. */
  type Constraints = HashMap[TypeID, StoredTypeConstraint]

  /** Mapping giving the type constraints on type parameters currently in
    * scope. */
  type TypeParamMap = HashMap[TypeParamName, TypeParamConstraint]
}
