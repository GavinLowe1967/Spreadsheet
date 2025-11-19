package spreadsheet

import FunctionType._

/** The type of functions from `domain` to `range`. 
  * @param params A list of free type identities, paired with a constraint upon
  * them. */
case class FunctionType(
  params: List[TypeParameter], domain: List[TypeT], range: TypeT
) extends TypeT{
  def asString = 
    domain.map(_.asString).mkString("(", ",", ")")+" => "+range.asString

  def typeParams = {
    assert(params.isEmpty)    // IMPROVE ???
    domain.flatMap(_.typeParams) ++ range.typeParams
  }

  /** Type parameters that are used not in domain. */
  val unusedTParams: List[TypeParameter] =
    params.filter{ case(p,c) => domain.forall(!_.typeParams.contains(p)) }

  /** Type parameters that are used in domain. */
  val usedTParams = params.filter(tp => !unusedTParams.contains(tp))
// IMPROVE: do we need both?

}

object FunctionType{
  /** The type parameters for functions. */
  type TypeParameter = (TypeParam.TypeParamName, TypeParamConstraint)
}
