package spreadsheet

/** A function defined by `f`.  The Environment argument of `f` gives the
  * call-time environment in which function is executed, and it used only by
  * built-in operations.  `fTParams` gives the names of formal type parameters. */
case class FunctionValue(f: Environment => PartialFunction[List[Value], Value], 
  fTParams: List[String])
    extends Value{
  /** Apply this to `args`. */
  def apply(env: Environment, args: List[Value], aTParams: List[TypeT]): Value = {
    // val env1 = 
    //   if(aTParams.nonEmpty) env.setTPs(fTParams, aTParams) else env
    val fe = f(env); assert(fe.isDefinedAt(args/*,aTParams*/)); fe(args/*, aTParams*/)
  }

  /** The actual type parameters associated with this. */
  private var aTParams = List[TypeT]()

  def getATParams = aTParams

  /** A clone of this but with actual type parameters tParams. */
  def addTParams(tParams: List[TypeT]): FunctionValue = {
    require(tParams.length == fTParams.length)
    val fv = FunctionValue(f, fTParams); fv.aTParams = tParams; fv
  }

  def forError = "<function>" 
}

