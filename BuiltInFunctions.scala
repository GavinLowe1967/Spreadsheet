package spreadsheet

/** Types and definitions of built-in functions. */
object BuiltInFunctions{
  /* Types. */

  private val headT = {
    // val tid = TypeVar.getNext(); val t = TypeVar(tid)
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), t)
  }
  private val tailT = {
    //val tid = TypeVar.getNext(); val t = TypeVar(tid)
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), ListType(t))
  }
  private val isEmptyT = { 
    //val tid = TypeVar.getNext(); val t = TypeVar(tid)
    val t = TypeParam("A")
    FunctionType(List(("A", AnyTypeConstraint)), List(ListType(t)), BoolType)
  }

  /** The types of built-in functions. */
  val builtInTypes = 
    List("head" -> headT, "tail" -> tailT, "isEmpty" -> isEmptyT)

  /* Definitions. */

  private val headFn = 
    BuiltInFunction{ case List(l:ListValue) => l.head }
  private val tailFn = 
    BuiltInFunction{ case List(l:ListValue) => l.tail }
  private val isEmptyFn = 
    BuiltInFunction{ case List(l:ListValue) => l.isEmpty }

  /** The built-in functions. */
  val builtIns = 
    List("head" -> headFn, "tail" -> tailFn, "isEmpty" -> isEmptyFn)

}
