package spreadsheet

import TypeVar.TypeID 

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
}

