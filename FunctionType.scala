package spreadsheet

/** The type of functions from `domain` to `range`. 
  * @param params A list of free type identities, paired with a constraint upon
  * them. */
case class FunctionType(
  params: List[FunctionType.TypeParameter], domain: List[TypeT], range: TypeT
) extends TypeT{
  def asString = 
    domain.map(_.asString).mkString("(", ",", ")")+" => "+range.asString

  def typeVars = domain.flatMap(_.typeVars) ++ range.typeVars

  def typeParams = {
    assert(params.isEmpty)    // IMPROVE ???
    domain.flatMap(_.typeParams) ++ range.typeParams
  }

}

object FunctionType{
  /** The type parameters for functions. */
  type TypeParameter = (TypeParam.TypeParamName, TypeParamConstraint)
}
