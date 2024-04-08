package spreadsheet

import scala.collection.mutable.HashMap


// ==================================================================

import TypeVar.TypeID

object Substitution{

  /** Apply the substitution tv -> t to t1. */ 
  def reMap(tv: TypeID, t: TypeT, t1: TypeT): TypeT = t1 match{
    case TypeVar(tv1) => if(tv1 == tv) t else t1
    case ListType(underlying) => ListType(reMap(tv, t, underlying))
    case FunctionType(params, domain, range) =>
      FunctionType(params, domain.map(reMap(tv, t, _)), reMap(tv, t, range))
    case _ => t1
  }

  import TypeParam.TypeParamName

  type TypeMap = HashMap[TypeParamName, TypeVar]

  /** Remap t according to typeMap, replacing TypeParams by TypeVars. */
  private def remapBy(typeMap: TypeMap, t: TypeT): TypeT = t match{
    case TypeParam(tp) => 
      typeMap.get(tp) match{ case Some(t1) => t1; case None => t }
    // case TypeVar(tv) =>
    //   typeMap.get(tv) match{ case Some(t1) => t1; case None => t }
    case ListType(underlying) => ListType(remapBy(typeMap, underlying))
    case FunctionType(params, domain, range) =>
      assert(params.forall{ case (tp,_) => !typeMap.contains(tp) })
// FIXME: other types
      FunctionType(params, domain.map(remapBy(typeMap, _)), remapBy(typeMap, range))
    case _ => t
  }


  /** Remap (ts,t) according to pairs. */
  def remapBy(pairs: List[(TypeParamName, TypeVar)], ts: List[TypeT], t: TypeT)
      : (List[TypeT], TypeT) = {
    val typeMap = new TypeMap ++ pairs
    (ts.map(remapBy(typeMap, _)), remapBy(typeMap, t))
  }
}



// ========= Dead code below. =========

/*
/** Abstractly, a mapping from type variables to type expressions (TypeT), as
  * captured by map. 
  * @param map The mapping from identifiers of type variables to the 
  * corresponding type expression.  Any identifier not in the map implicitly
  * maps to itself.*/
class Substitution(private var map: TypeMap){

  /** The value associated with tv.  Testing only. */
  private def apply(tv: TypeID) = map(tv)

  /** Clear the map.  For testing only. */
  private def clear() = map.clear()

  /** Apply this substitution to t. */
  def applyTo(t: TypeT): TypeT = t match{
    case TypeVar(tv) => 
      map.get(tv) match{ case Some(t1) => t1; case None => t }
    case ListType(underlying) => ListType(applyTo(underlying))
    case FunctionType(domain, range) => 
      FunctionType(domain.map(applyTo), applyTo(range))
    case _ => t
  }

  /** Extend this according to the constaint TypeVar(tv) = t, unless that
    * creates an infinite type. */
  private def extend(tv: TypeID, t: TypeT): Reply[Unit] = {
    require(!map.contains(tv))
    require(applyTo(t) == t)
    if(t == TypeVar(tv)) Ok(())
    else if(t.typeVars.contains(tv)) FailureR(s"Can't unify $t with $tv")
    else{ 
      // For each (tv1 -> t1) in map, apply the substitution tv -> t to t1.
      map = map.map{ case (tv1,t1) => (tv1, Substitution.reMap(tv, t, t1)) } 
      map += tv -> t; Ok(())
    } 
  }

  /** Extend this to unify t1 and t2. */
  def unify(t1: TypeT, t2: TypeT): Reply[Unit] = (t1,t2) match{
    case (TypeVar(tv), _) => 
      map.get(tv) match{
        case None => extend(tv, applyTo(t2))
        case Some(t11) => unify(t11, applyTo(t2))
      }

    case (_, TypeVar(_)) => unify(t2,t1) 
    
    case (IntType, IntType) | (BoolType, BoolType) | (StringType, StringType) =>
      Ok(())

    case (ListType(u1), ListType(u2)) => unify(u1, u2)

    case (FunctionType(d1,r1), FunctionType(d2,r2)) => 
      val len = d1.length
      if(len != d2.length) FailureR(s"Cannot unify $t1 with $t2")
      else{
        var res = unify(r1,r2); var i = 0
        while(i < len && res == Ok(())){
          res = unify(d1(i), d2(i)); i += 1
        }
        res
      }

    case _ => FailureR(s"Cannot unify $t1 with $t2")
  }

}
 */

/*
  /** Testing. */
  def main(args: Array[String]) = {
    val sub = new Substitution(new TypeMap)
    def ok(r: Reply[Unit]) = r == Ok(())
    assert(ok(sub.unify(FunctionType(List(IntType), BoolType), TypeVar(0))))
    assert(!ok(sub.unify(TypeVar(0), IntType)))
    assert(ok(sub.unify(TypeVar(0), FunctionType(List(TypeVar(1)), TypeVar(2)))))
    assert(sub(1) == IntType); assert(sub(2) == BoolType)
    assert(!ok(sub.unify(FunctionType(List(TypeVar(3)), BoolType), TypeVar(3))))
    assert(!ok(sub.unify(FunctionType(List(IntType), TypeVar(3)), TypeVar(3))))

    sub.clear()
    assert(ok(sub.unify(TypeVar(0), FunctionType(List(TypeVar(1)), TypeVar(2)))))
    assert(ok(sub.unify(TypeVar(1), IntType)))
    assert(ok(sub.unify(TypeVar(3), TypeVar(2))))
    assert(ok(sub.unify(TypeVar(2), TypeVar(3))))
    assert(ok(sub.unify(TypeVar(3), BoolType)))
    assert(sub(0) == FunctionType(List(IntType), BoolType))

    println("Done")
  }
 */

