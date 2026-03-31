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
    domain.map(_.asString).mkString("(", ",", ")")+" => "+range.asString

  def typeParams = 
    (params.map(_._1) ++ domain.flatMap(_.typeParams) ++ range.typeParams
    ).distinct

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
}

object FunctionType{
  /** The type parameters for functions. */
  type TypeParameter = (TypeParam.TypeParamName, TypeParamConstraint)
}
