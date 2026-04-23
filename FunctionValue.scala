package spreadsheet

/** A function defined by `f`.  The Environment argument of `f` gives the
  * call-time environment in which function is executed, and it used only bu
  * built-in operations. */
case class FunctionValue(f: Environment => PartialFunction[List[Value], Value]) 
    extends Value{
  /** Apply this to `args`. */
  def apply(env: Environment, args: List[Value]): Value = {
    val fe = f(env); assert(fe.isDefinedAt(args)); fe(args)
  }

  def forError = "<function>" 
}

