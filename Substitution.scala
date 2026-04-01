package spreadsheet

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
    case TupleType(cpts) => TupleType(cpts.map(reMap(tv, t, _)))
    case _: BaseType | _: TypeParam | _: CellTypeVar | null => t1 
  }

  /* We substitute TypeParams with TypeVars, but store the TypeParamConstraint
   * associated with the TypeParam. */

  private type TypeVarParamConstraint = (TypeVar, TypeParamConstraint)

  /** Type of substitutions of type parameters by type variables.  Also store
    * the constraint associated with each type parameter. */
  type TypeMap = Map[TypeParamName, TypeVarParamConstraint]

  val emptyTypeMap = Map[TypeParamName, TypeVarParamConstraint]()

  /** Remap t according to typeMap, replacing TypeParams by TypeVars. */
  def remapBy(typeMap: TypeMap, t: TypeT): TypeT = t match{
    case TypeParam(tp) => 
      typeMap.get(tp) match{ case Some((t1,_)) => t1; case None => t }

    case ListType(underlying) => ListType(remapBy(typeMap, underlying))

    case FunctionType(params, domain, range) =>
      assert(params.forall{ case (tp,_) => !typeMap.contains(tp) })
      FunctionType(params, domain.map(remapBy(typeMap, _)), 
        remapBy(typeMap, range))

    case TupleType(cpts) => TupleType(cpts.map(t1 => remapBy(typeMap, t1))) 

    case _: BaseType | _: TypeVar => t
  }

//  def remapTypeVars(f: PartialFunction[TypeID, TypeConstraint]], t: Type

  /** Create a remapping from the type parameters in tParams to fresh TypeVars,
    * together with corresponding constraints on those TypeVars. */
  private def mkRemapping(tParams: List[TypeParameter])
      : (TypeMap, List[(TypeID, TypeConstraint)]) = {
    var pairs = List[(TypeParamName, TypeVarParamConstraint)]() 
    var constraints = List[(TypeID, TypeConstraint)]()
    for((p,c) <- tParams){
      val tId = nextTypeID(); pairs ::= ((p, (TypeVar(tId), c)))
      constraints ::= (tId,c)
    }
    (Map.from(pairs), constraints)
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
      typeMap.get(tp) match{ case Some((t1,_)) => t1; case None => t }
    case ListType(underlying) => 
      ListType(remapInResult(tParams, typeMap, underlying))
    case FunctionType(params, domain, range) =>
      assert(params.forall{ case (n,_) => !tParams.map(_._1).contains(n) })
      FunctionType(params++tParams, domain, range)
    case TupleType(cpts) => 
      TupleType(cpts.map(t1 => remapInResult(tParams, typeMap, t1)))
    case tv: TypeVar => tv // println(getConstraint(tv)); ???
    case _: BaseType | _: CellTypeVar => t 
  }

  /** Substitute type parameters in result from tParams with fresh type
    * variables.  Add tParams as parameters of any FunctionType.  */
  def subTypeParamsInResult(
    typeEnv: TypeEnv, tParams: List[TypeParameter], result: TypeT)
      : (TypeEnv, TypeT) = {
    val (typeMap, constraints) = mkRemapping(tParams)
    (typeEnv.addConstraints(constraints), 
      remapInResult(tParams, typeMap, result))
  }

  // ========= Reverse mapping

  /** Maps from type variables to type parameters. */
  type ReverseTypeMap = Map[TypeVar, TypeParameter] 

  /** An empty ReverseTypeMap. */
  val emptyRevMap: ReverseTypeMap = Map[TypeVar, TypeParameter]()

  /** Union of m1 and m2.  Pre: they agree on common TypeVars. */
  def union(m1: ReverseTypeMap, m2: ReverseTypeMap): ReverseTypeMap = {
    // Check m1 and m2 agree on common TypeVars
    for((tv,tp) <- m2) m1.get(tv) match{
      case Some(tp1) => assert(tp == tp1); case None => {}
    }
    m1 ++ m2
  }

  /** Remap t according to rtMap, adding type parameters in the range of
    * rtMap. */
  def reverseRemapBy(rtMap: ReverseTypeMap, t: TypeT): TypeT = {
    val tParams = rtMap.toList.sortBy{ case ((TypeVar(tv),_)) => tv }.map(_._2)
    // Note: the "sortBy" sorts the type parameters back to their original
    // order.
    reverseRemapBy(rtMap, tParams, t)
  }

  /** Remap t according to rtMap, adding tParams as type parameters. */
  def reverseRemapBy(
    rtMap: ReverseTypeMap, tParams: List[TypeParameter], t: TypeT)
      : TypeT = t match{
    case tv: TypeVar => 
      rtMap.get(tv) match{ case Some((tp,_)) => TypeParam(tp); case None => t }
    case ListType(u) => ListType(reverseRemapBy(rtMap, tParams, u))
    case FunctionType(params, domain, range) =>
      assert(tParams.forall{ case (n,_) => !t.typeParams.contains(n) })
      FunctionType(params++tParams, domain.map(reverseRemapBy(rtMap, List(), _)),
        reverseRemapBy(rtMap, List(), range))
    case TupleType(cptTs) => 
      TupleType(cptTs.map(t1 => reverseRemapBy(rtMap, tParams, t1)))
    case _: BaseType | _: TypeVar | _: CellTypeVar | _: TypeParam => t
  }

  /** The inverse of map.  Assumes that map is injective on parameter names. */
  def inverse[A,B](map: TypeMap): ReverseTypeMap = {
    assert(map != null)
    if(map eq emptyTypeMap) emptyRevMap
    else{
      // Check type parameter names in range are distinct.
      val ys = map.toList.map{ case (x,(y,c)) => y }; assert(ys.distinct == ys)
      Map.from(for((x,(y,c)) <- map) yield y -> (x,c))
    }
  }

}

