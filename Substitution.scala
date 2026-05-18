package spreadsheet

import TypeVar.TypeID 
import TypeParam.TypeParamName

object Substitution{
  /** Apply the substitution tv -> t to t1. */ 
  def reMap(tv: TypeID, t: TypeT, t1: TypeT): TypeT = t1 match{
    case TypeVar(tv1) => if(tv1 == tv) t else t1
    case ListType(underlying) => ListType(reMap(tv, t, underlying))
    case FunctionType(params, domain, range) =>
      FunctionType(params, domain.map(reMap(tv, t, _)), reMap(tv, t, range))
    case TupleType(cpts) => TupleType(cpts.map(reMap(tv, t, _)))
    case _: BaseType | _: TypeParam | _: CellTypeVar | null => t1 
  }

  /** Substitute all type parameters in t according to map. */
  def substitute(map: Map[TypeParamName, TypeT], t: TypeT): TypeT = t match{
    case TypeParam(tp) => 
      map.get(tp) match{ case Some(t1) => t1; case None => t }
    case ListType(underlying) => ListType(substitute(map, underlying))
    case FunctionType(params, domain, range) =>
      val map1 = map -- params.map(_._1)
      FunctionType(
        params, domain.map(substitute(map1,_)), substitute(map1, range))
    case TupleType(cpts) => TupleType(cpts.map(substitute(map,_)))
    case _: BaseType | _: TypeVar | _: CellTypeVar | null => t
  }

}

