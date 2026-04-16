package spreadsheet

/** Tests involving parsing, type checking and evaluation of expressions,
  * independent of any cell values. */
object EvaluationTest{
  val expr = StatementParser.expParser.expr
  val typeCheckAndClose = TypeChecker.etc.typeCheckAndClose _

  /** Parse, type check and evaluate st.  The parsing and type checking are
    * expected to succeed; but evaluation may fail. */
  def eval(st: String): Value = {
    val e = Parser.parseAll(expr, st); val env = TypeEnv()
    typeCheckAndClose(env, e) match{
      case Ok((te,t)) => 
        val env = Environment(100,26); Execution.TestHooks.eval(env,e)
      case FailureR(err) => println(err); null
    }
  }

  /** Assert that v is an ErrorValue. */
  def assertFail(v: Value) = v match{
    case _: ErrorValue => {}; case _ => sys.error(s"Expected error, found $v")
  }

  /** Tests on basic expressions. */
  private def tests1() = {
    // Arithmetic expressions
    assert(eval("123.45") == FloatValue(123.45F))
    assert(eval("-456.12") == FloatValue(-456.12F))
    assert(eval("2+3") == IntValue(5))
    assert(eval("2+3*4") == IntValue(14)); assert(eval("2*3+4") == IntValue(10))
    assert(eval("(2+3)") == IntValue(5))
    assert(eval("12%5") == IntValue(2)); assertFail(eval("3%0"))
    assert(eval("(2+3)*4 == 6") == BoolValue(false))
    assert(eval("(1+4)*4 == 60/3") == BoolValue(true))
    assert(eval("(2+3)*4 != 6") == BoolValue(true))
    assert(eval("(2+3)*4 != 60/3") == BoolValue(false))
    assert(eval("(2+3)*4 > 6") == BoolValue(true))
    assert(eval("(2+3)*4 <= 6 || 6*7 == 42") == BoolValue(true))
    assert(eval("(2+3)*4 <= 6 && 6*7 == 42") == BoolValue(false))
    assertFail(eval("3/0+4")); assertFail(eval("2+5/0"))
    assert(eval("()") == UnitValue)
    // "to" and "until"
    assert(eval("3 to 5") == ListValue(List(3,4,5).map(IntValue)))
    assert(eval("3 until 5") == ListValue(List(3,4).map(IntValue)))
    assert(eval("#3 to #5") == ListValue(List(3,4,5).map(RowValue)))
    assert(eval("#3 until #5") == ListValue(List(3,4).map(RowValue)))
    assert(eval("#D to #F") == ListValue(List(3,4,5).map(ColumnValue(_))))
    assert(eval("#D until #F") == ListValue(List(3,4).map(ColumnValue(_))))
    assertFail(eval("3 to head([])"))
    // Row and column arithmetic
    assert(eval("#D+3") == eval("#G")); assert(eval("#4+2") == eval("#6"))
    assert(eval("#D-2") == eval("#B")); assert(eval("#7-3") == eval("#4"))
    assertFail(eval("#B-4")); assertFail(eval("#3-4"))
    assert(eval("#5-#2") == IntValue(3)); assert(eval("#E-#B") == IntValue(3))
    // Tyeval conversion
    assert(eval("toInt 4.5") == IntValue(4))
    assert(eval("toFloat 3") == FloatValue(3.0F))
    // Tuples
    assert(eval("(2,3.5)") == TupleValue(IntValue(2), FloatValue(3.5F)))
    assert(eval("!(2+2 == 4)") == BoolValue(false))
    assert(eval("- (3)") == IntValue(-3))
    assert(eval("-{ val x = 4; x+6}") == IntValue(-10))
    assert(eval("-(3.6-2.6)") == FloatValue(-1.0F))
  }

  /** Blocks, if statements, list expressions. */
  private def tests2() = {
    // ===== Blocks
    assert(eval("{ val x = 3; x+17 }") == IntValue(20))
    assert(eval("{ val x = 3 \n x+4 }") == IntValue(7))
    assert(eval("{ 4*5 }") == IntValue(20))
    assert(eval("{ #B3 = 4; 42 }") == IntValue(42))
    assert(eval("{ #B3 = 4 }") == UnitValue)
    // ===== if statements
    assert(eval("if(2+2 == 4) 3 else 4+2") == IntValue(3))
    assert(eval("if(2+2 == 5) 3 else 4+2") == IntValue(6))
    assert(eval("if(2+2 != 5) 3 else 4+2") == IntValue(3))
    assert(eval("7 * (if(2+2 == 4) 3 else 4+2)") == IntValue(21))
    assert(eval("7 * (if(2+2 == 5) 3 else 4+2)") == IntValue(42))
    assertFail(eval("if(2/0 == 4) 3 else 4"))
    // ===== List expressions
    assert(eval("[]") == ListValue(/*AnyTyeval,*/ List()))
    assert(eval("[4/4, 2+0, 6-3]") == 
      ListValue(IntValue(1), IntValue(2), IntValue(3)))
    assertFail(eval("[4/2, 3/0]"))
    assert(eval("head([1,2,3])") == IntValue(1))
    assertFail(eval("head([])"))
    assert(eval("tail([1,2,3])") == ListValue(IntValue(2), IntValue(3)))
    assertFail(eval("tail([])"))
    assert(eval("[1,2] == [3,4]") == BoolValue(false))
    assert(eval("1 :: 2 :: []") == ListValue(IntValue(1), IntValue(2)))
    assert(eval("[1,2] != tail([3,1,2])") == BoolValue(false))
    assert(eval("[1,2] == tail([3,1,2])") == BoolValue(true))
    assert(eval("tail([1]) == []") == BoolValue(true))
    assert(eval("[] == tail([1])") == BoolValue(true))
    assert(eval(
      "{val xs = [x+y | x <- [1,2,3], y <- [4,7], x != 2]; xs == [5,8,7,10]}"
    ) == BoolValue(true))
    // ===== Pairs
    assert(eval("{val pair = (2,3.5); get1 pair}") == IntValue(2))
    assert(eval("get1((2,4,6,8))") == IntValue(2))
    assert(eval("get4((2,4,6,8))") == IntValue(8))
  }

  /** Tests involving functions. */
  private def tests3() = {
    assert(
      eval("{def fact(n: Int) : Int = if(n <= 1) 1 else n*fact(n-1)\n"+
        "val ff = fact: Int => Int; ff(3)}"
      ) == IntValue(6) )
    // Test of scoping and forward reference
    assert(
      eval("{val y = 3; val res = g(1)\n" + // should be 4
        "def g(z: Int): Int = { val y = 10; f z }\n" +
        "def f(x: Int): Int = x+y; res}"
      ) == IntValue(4))
    // Currying, higher-order function
    assert(
      eval("{def foldr[A,B](f: A => B => B)(e: B)(xs: List[A]): B = "+
        "if(isEmpty xs) e else f(head xs)(foldr f e (tail xs)) \n"+
        "val sum = { def p(x:Int)(y:Int) = x+y; foldr p 0 } \n" +
        "sum [1,4]: Int + sum([]: List[Int])}"
      ) == IntValue(5))

    // Overloading
    assert(
      eval("{def f1(x: Int) = x+1; def f1[A](x: A) = x; (f1(3), f1(true))}") ==
        TupleValue(IntValue(4), BoolValue(true))) // 4, true
    assert(
      eval("{def f2[A](x: A) = x; def f2(x: Int) = x+1; (f2(3), f2(true))}") ==
      TupleValue(IntValue(3), BoolValue(true))) // 3, true
    assert(
      eval("{def sum(xs: List[Int]): Int = "+
        "  if(isEmpty xs) 0 else head xs + sum(tail(xs)) \n" +
        "def sum(xs: List[Float]): Float = "+
        "  if(isEmpty xs) 0.0 else head xs + sum(tail(xs)) \n" +
        "val s = sum: List[Int] => Int; "+
        "(sum[2.4,4.6], s [2,4]) }"
      ) == TupleValue(FloatValue(7.0F), IntValue(6)) )
  }

  /** Tests involving assertions. */
  private def tests4() = {
    assert(eval("{ assert(2+2==4); 3 }") == IntValue(3))
    // "Assertion error at line 1 in "assert(false)" in ..." 
    assertFail(eval("{ assert(false); 3 }"))
    // "Division by zero at line 1 in ..."
    assertFail(eval("{ assert(1/0 != 3); 3 }"))

    assert(eval("{ assert(true, \"XX\"); 3 }") == IntValue(3))
    // "Assertion error: XX at line 1 in "assert(false, "XX")" in ..."
    assertFail(eval("{ assert(false, \"XX\"); 3 }"))
    assertFail(eval("{ assert(1/0 != 3, \"XX\"); 3 }"))
    assertFail(eval("{ assert(false, if(1/0 != 3) \"XX\" else \"YY\"); 3 }"))
  }

  private def scriptTests() = {
    // def mkScript(st: String) = s"{#include \"haskell.dir\" \n $st }"
    // def eval1(st: String) = eval(mkScript(st))
    // assert(eval1("nonEmpty []"))
    val Height = 100; val Width = 26
    val model = new Model(Height,Width); model.setView(DummyView)
    val env = model.getEnv; val isCalculated = env.isCalculated _
    model.loadScript("evaluationTest.dir", null)
    assert(env("b1") == BoolValue(false) && env("b2") == BoolValue(true))
    assert(env("b3") == BoolValue(true) && env("b4") == BoolValue(false))
    assert(env("b5") == BoolValue(false)) // lazy evaluation of &&
  }


  def main(args: Array[String]) = {
    println("===EvaluationTest===")
    tests1() // basic expressions
    tests2() // blocks, if statements, list expressions
    tests3() // functions
    tests4()
    scriptTests()
  }



}
