package spreadsheet

/** A function defined by `f`. */
case class FunctionValue(f: FunctionValue.FunctionType) extends Value{
  /** Apply this to `args`. */
  def apply(env: Environment, args: List[Value]): Value = {
    val fe = f(env)(aTParams); assert(fe.isDefinedAt(args)); fe(args)
  }

  /** The actual type parameters associated with this.  Set by
    * FunctionValue.apply when the script provides actual type parameters. */
  private var aTParams = List[TypeT]()

  def forError = "<function>" 
}

object FunctionValue{
  /** The type of the functions encapsulated in a FunctionValue object.  The
    * Environment argument of `f` is the call-time environment in which the
    * function is executed; it is used only by built-in operations.  The
    * second argument is the list of actual type parameters.  This produces a
    * PartialFunction that takes the actual parameters and returns the result
    * of the script function. */
  type FunctionType = 
    Environment => List[TypeT] => PartialFunction[List[Value], Value]

  /** A FunctionValue with actual type parameters aTParams. */
  def apply(f: FunctionType, aTParams: List[TypeT]): FunctionValue = {
    val fv = FunctionValue(f); fv.aTParams = aTParams; fv
  }
}
