package spreadsheet

import scala.collection.mutable.HashMap
import TypeVar.{TypeID,nextTypeID}
import TypeParam.TypeParamName 
import FunctionType.TypeParameter

object Substitution{

  /** Apply the substitution tv -> t to t1. */ 
  def reMap(tv: TypeID, t: TypeT, t1: TypeT): TypeT = t1 match{
    case TypeVar(tv1) => if(tv1 == tv) t else t1
    case ListType(underlying) => ListType(reMap(tv, t, underlying))
    case FunctionType(params, domain, range) =>
      FunctionType(params, domain.map(reMap(tv, t, _)), reMap(tv, t, range))
    case _ => t1
  }

  /** Type of substitutions of type parameters by type variables. */
  type TypeMap = HashMap[TypeParamName, TypeVar]

  /** Remap t according to typeMap, replacing TypeParams by TypeVars. */
  def remapBy(typeMap: TypeMap, t: TypeT): TypeT = t match{
    case TypeParam(tp) => 
      typeMap.get(tp) match{ case Some(t1) => t1; case None => t }

    case ListType(underlying) => ListType(remapBy(typeMap, underlying))

    case FunctionType(params, domain, range) =>
      assert(params.forall{ case (tp,_) => !typeMap.contains(tp) })
// FIXME: other types
      FunctionType(params, domain.map(remapBy(typeMap, _)), 
        remapBy(typeMap, range))

    case _ => t
  }

  /** Create a remapping from the type parameters in tParams to fresh TypeVars,
    * as a list, together with corresponding constraints on those TypeVars. */
  private def mkRemapping(tParams: List[TypeParameter])
      : (TypeMap, List[(TypeID, TypeConstraint)]) = {
    val typeMap = new TypeMap
    var constraints = List[(TypeID, TypeConstraint)]()
    for((p,c) <- tParams){
      val tId = nextTypeID(); typeMap += ((p, TypeVar(tId)))
      constraints ::= (tId,c)
    }
    (typeMap, constraints)
  }

  /** Given a type domainT => rangeT for a function, replace the type parameters
    * in typParams by fresh type variables, adding corresponding constraints
    * to typeEnv. */
  def replaceTypeParamsByTypeVars(typeEnv: TypeEnv, tParams: List[TypeParameter],
                    domain: List[TypeT], range: TypeT)
      : (TypeEnv, List[TypeT], TypeT) = {
    // Create fresh type variables to replace tParams in domain and range
    val (typeMap, constraints) = mkRemapping(tParams)
    (typeEnv.addConstraints(constraints), domain.map(remapBy(typeMap, _)),
       remapBy(typeMap, range))
  }

  /** Remap, in t, the type parameters in tParams according to pairs.  tParams
    * are added as type parameters in any FunctionTypes, and not recursively
    * remapped.  Pre: tParams is the domain of typeMap */ 
  private 
  def remapInResult(tParams: List[TypeParameter], typeMap: TypeMap, t: TypeT)
      : TypeT = t match{
    case TypeParam(tp) => 
      typeMap.get(tp) match{ case Some(t1) => t1; case None => t }
    case ListType(underlying) => 
      ListType(remapInResult(tParams, typeMap, underlying))
    case FunctionType(params, domain, range) =>
      assert(params.forall{ case (tp,_) => !typeMap.contains(tp) })
      FunctionType(params++tParams, domain, range)
    case _ => t
  }

  /** Substitute type parameters in result from tParams with fresh type
    * variables. */
  def subTypeParamsInResult(
    typeEnv: TypeEnv, tParams: List[TypeParameter], result: TypeT)
      : (TypeEnv, TypeT) = {
    val (typeMap, constraints) = mkRemapping(tParams)
    (typeEnv.addConstraints(constraints), 
      remapInResult(tParams, typeMap, result))
  }
}

