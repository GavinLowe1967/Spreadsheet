package spreadsheet

import scala.collection.mutable.HashMap
import TypeVar.TypeID
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

  /** Make a TypeMap based on pairs. */
  def mkTypeMap(pairs: List[(TypeParamName, TypeVar)]) = new TypeMap ++ pairs

  /** Remap (ts,t), replacing type parameters by type variables according to
    * pairs. */
  def remapBy(pairs: List[(TypeParamName, TypeVar)], ts: List[TypeT], t: TypeT)
      : (List[TypeT], TypeT) = {
    val typeMap = mkTypeMap(pairs) // new TypeMap ++ pairs
    (ts.map(remapBy(typeMap, _)), remapBy(typeMap, t))
  }

  /** Remap, in t, the type parameters in tParams according to pairs.  tParams
    * are added as type parameters in any FunctionTypes, and not recursively
    * remapped.  Pre: tParams is the domain of typeMap */ 
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
}

