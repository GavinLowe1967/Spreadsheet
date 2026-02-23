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

  /** Given a type domain => range for a function, replace the type parameters
    * in typParams by fresh type variables, adding corresponding constraints
    * to typeEnv.
    * @return the updated type environment; the result of remapping domain; the
    * result of remapping range; the TypeMap used.  */
  def replaceTypeParamsByTypeVars(typeEnv: TypeEnv, tParams: List[TypeParameter],
                    domain: List[TypeT], range: TypeT)
      : (TypeEnv, List[TypeT], TypeT, TypeMap) = {
    // Create fresh type variables to replace tParams in domain and range
    val (typeMap, constraints) = mkRemapping(tParams)
    (typeEnv.addConstraints(constraints), domain.map(remapBy(typeMap, _)),
       remapBy(typeMap, range), typeMap)
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

  // ========= Reverse mapping

  /** Maps from type variables to type parameters. */
  private type ReverseTypeMap = HashMap[TypeVar, TypeParamName]

  /** Remap t according to rtMap, adding tParams as type parameters. */
  private def reverseRemapBy(
    rtMap: ReverseTypeMap, tParams: List[TypeParameter], t: TypeT
  ): TypeT = t match{
    case tv: TypeVar => 
      rtMap.get(tv) match{ case Some(tp) => TypeParam(tp); case None => t }
    case ListType(u) => ListType(reverseRemapBy(rtMap, tParams, u))
    case FunctionType(params, domain, range) =>
      // FIXME: remove repetitions? 
      FunctionType(params++tParams, domain.map(reverseRemapBy(rtMap, List(), _)),
        reverseRemapBy(rtMap, List(), range)) 
    case t => t
  }

/*
  /** Normalise t, adding type parameters in the top-level FunctionType. */
  private def normalise(t: TypeT): TypeT = t match{
    case TypeParam(_) => ??? // I think this shouldn't happen
    case ListType(u) => ListType(normalise(u))
    case FunctionType(params, domain, range) => ???
    case t => t
  }

  /** Find all type parameters in t, and separate them out.  This is called when
    * t is part of a FunctionType ft; the type parameters need to be added to
    * ft. */
  private def normalise1(t: TypeT): (TypeT, Set[TypeParamName]) = t match{
    case TypeParam(tp) => (t, Set(tp))
    case ListType(u) => val (u1, ps) = normalise1(u); (ListType(u1), ps)
    case FunctionType(params, domain, range) => 
      val dd = domain.map(normalise1); val (rt,rps) = normalise1(range)
      val dps: Set[TypeParamName] = Set.concat(dd.map(_._2))
      val ps = Set.from(params).union(dps).union(rps)
      (FunctionType(List(), dd.map(_._1), rt), ps)
  }
 */



  /** The inverse of map.  Assumes that map is injective. */
  def inverse[A,B](map: HashMap[A,B]): HashMap[B,A] = {
    assert(map != null)
    val newMap = new HashMap[B,A]
    for((x,y) <- map){ assert(!newMap.contains(y)); newMap += y -> x }
    newMap
  }

  /** Remap type variables back to type parameters by the inverse of map, adding
    * tParams as type parameters. */ 
  def remapTypeVarsBackToTypeParams(
    map: ReverseTypeMap, tParams: List[TypeParameter], t: TypeT
  ): TypeT =
    reverseRemapBy(map, tParams, t)
// IMPROVE: we don't need this function -- use reverseMapBy directly

}

