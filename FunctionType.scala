package spreadsheet

import FunctionType._
import TypeParam.{TypeParamName, TypeParamMap}

/** The type of functions from `domain` to `range`. 
  * @param params A list of free type identities, paired with a constraint upon
  * them. */
case class FunctionType(
  params: List[TypeParameter], domain: List[TypeT], range: TypeT
) extends TypeT{
  def asString = 
    domain.map(_.asString).mkString("(", ",", ")")+" => "+
      (if(range == null) "<undefined type>" else range.asString)

  private val domainTParams: List[String] = domain.flatMap(_.typeParams).distinct

  // /** The type parameter names in domain and range. */
  // private def typeParams0: List[String] = (
  //   domain.flatMap(_.typeParams) ++ (if(range == null) List() else range.typeParams)).distinct

  /** The type parameter names in params, domain and range. */
  def typeParams: List[String] = 
    (params.map(_._1) ++ domainTParams ++ range.typeParams).distinct

  def typeVars = (domain.flatMap(_.typeVars) ++ range.typeVars).distinct

  def renameTypeParams(f: TypeParamMap, tps: Set[TypeParamName]) = {
    // Extend f to map any new parameter names that clash with an element of
    // tps to a new name.
    val f1 = f ++ (
      for((n,_) <- params; if tps.contains(n))
      yield n -> TypeParam.getNewName(n) )
    FunctionType(
      params.map{ case (v,c) => (f1.getOrElse(v, v), c) },
      domain.map(_.renameTypeParams(f1,tps)), range.renameTypeParams(f1,tps)
    )
  }

  /** Type parameters that are used not in domain. */
  val unusedTParams: List[TypeParameter] =
    params.filter{ case(p,c) => domain.forall(!_.typeParams.contains(p)) }

  /** Type parameters that are used in domain. */
  val usedTParams = params.filter(tp => !unusedTParams.contains(tp))

  /** Does ft match this, but with a null final return type? */ 
  def matches(ft: FunctionType): Boolean = {
    val FunctionType(ps1, d1, r1) = ft
    ps1 == params && d1 == domain && (range match{
      // If both return FunctionTypes, recurse.
      case rft: FunctionType => 
        r1 match{
          case rft1: FunctionType => rft.matches(rft1)
          case null => true; case _ => false
        }
      case _ => r1 == null
    })
  }

  /** Does this (possibly curried) function have an undefined return? */
  def finalNull: Boolean = range match{
    case null => true; case ft: FunctionType => ft.finalNull; case _ => false
  }

  def hasNullReturnFunction = finalNull

  /** Does use of this require a concrete type parameter? */
  def needsConcreteTParam = {
    val tvs = domainTParams.distinct
    params.exists{ case (tp,tc) => !tvs.contains(tp) }
  }

/*
  /** Are numParams vectors of value parameters enough to define all the formal
    * type parameters? */
  def sufficientParams(numParams: Int): Boolean = 
    params.isEmpty || numParams >= neededParamsFor(params.map(_._1))

  /** How many vectors of value parameters are necessary to define the formal
    * type parameters `fTParams`?  Or a value of at least
    * FunctionType.Infinity if concrete type parameters will always be
    * necessary. */ 
  private def neededParamsFor(fTParams: List[String]): Int = {
    require(fTParams.nonEmpty)
    val missing = fTParams.filter(!domainTParams.contains(_))
    if(missing.isEmpty) 1
    else range match{
      case ft: FunctionType => 1+ft.neededParamsFor(missing)
      case _ => FunctionType.Infinity
    }
  }
 */
}

object FunctionType{
  /** The type parameters for functions. */
  type TypeParameter = (TypeParam.TypeParamName, TypeParamConstraint)

  /** Representation of infinity for use in neededParamsFor.  It is assumed that
    * no function will ever have this number of vectors of value
    * parameters! */
  //private val Infinity = 1 << 30
}
