package spreadsheet

/** Types and definitions of built-in functions. */
object BuiltInFunctions{
  /* Types. */

  private val headT = {
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), t)
  }
  private val tailT = {
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), ListType(t))
  }
  private val isEmptyT = { 
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), BoolType)
  }
  private val notT = FunctionType(List(), List(BoolType), BoolType)
  private val toIntT = FunctionType(List(), List(FloatType), IntType)
  private val toFloatT = FunctionType(List(), List(IntType), FloatType)

  /** The types of built-in functions. */
  val builtInTypes = 
    List("head" -> headT, "tail" -> tailT, "isEmpty" -> isEmptyT, "not" -> notT,
      "toInt" -> toIntT, "toFloat" -> toFloatT)

  /* Definitions. */

  private val headFn = 
    FunctionValue{ case List(l:ListValue) => l.head }
  private val tailFn = 
    FunctionValue{ case List(l:ListValue) => l.tail }
  private val isEmptyFn = 
    FunctionValue{ case List(l:ListValue) => l.isEmpty }
  private val notFn = FunctionValue{ case List(BoolValue(b)) => BoolValue(!b) }
  private val toIntFn = 
    FunctionValue{ case List(FloatValue(x)) => IntValue(x.toInt) }
  private val toFloatFn = 
    FunctionValue{ case List(IntValue(n)) => FloatValue(n.toFloat) }

  /** The built-in functions. */
  val builtIns = List(
    "head" -> headFn, "tail" -> tailFn, "isEmpty" -> isEmptyFn, "not" -> notFn,
    "toInt" -> toIntFn, "toFloat" -> toFloatFn
  )

}
