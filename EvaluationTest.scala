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
        val env = Environment(100,26,Model.initNameMap); 
        Execution.TestHooks.eval(env,e)
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
    assert(eval("() == () && !( () < () )") == BoolValue(true))
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
    assert(eval("(1,2.3) != (1,2.4) && (1,2.3) == (1,2.3)") == BoolValue(true))
    assert(eval("!(2+2 == 4)") == BoolValue(false))
    assert(eval("- (3)") == IntValue(-3))
    assert(eval("-{ val x = 4; x+6}") == IntValue(-10))
    assert(eval("-(3.6-2.6)") == FloatValue(-1.0F))
    assert(eval("(2, 4.4) < (2, 5.0) && (#4, 2.3) >= (#4, 2.3) "+
      " && (1,(2,3)) < (1,(2,4))") == BoolValue(true))
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
    //assert(eval("[]") == ListValue(/*AnyTyeval,*/ List()))
    assert(eval("[]: List[Int]") == ListValue(/*AnyTyeval,*/ List()))
    assert(eval("[4/4, 2+0, 6-3]") == 
      ListValue(IntValue(1), IntValue(2), IntValue(3)))
    assertFail(eval("[4/2, 3/0]"))
    assert(eval("head([1,2,3])") == IntValue(1))
    assertFail(eval("head([]: List[Int])"))
    assert(eval("tail([1,2,3])") == ListValue(IntValue(2), IntValue(3)))
    assertFail(eval("tail([]: List[Int])"))
    assert(eval("[1,2] == [3,4]") == BoolValue(false))
    assert(eval("1 :: 2 :: []") == ListValue(IntValue(1), IntValue(2)))
    assert(eval("[1,2] != tail([3,1,2])") == BoolValue(false))
    assert(eval("[1,2] == tail([3,1,2])") == BoolValue(true))
    assert(eval("tail([1]) == []") == BoolValue(true))
    assert(eval("[]: List[Int] == tail([1])") == BoolValue(true))
    assert(eval(
      "{val xs = [x+y | x <- [1,2,3], y <- [4,7], x != 2]; xs == [5,8,7,10]}"
    ) == BoolValue(true))
    assert(eval(
      "{val xs = [x+y | (x,y) <- [(1,4),(2,3),(3,4)], x != 2]; xs == [5,7]}"
    ) == BoolValue(true))
    assert(eval("[1,2] <= [1,3] && []: List[Float] < [3.6] && [\"hello\"] >= [\"hello\"] "+
      " && [#4] > [#3]") == BoolValue(true))


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
    // assert(
    //   eval("{def f1(x: Int) = x+1; def f1[A](x: A) = x; (f1(3), f1(true))}") ==
    //     TupleValue(IntValue(4), BoolValue(true))) // 4, true
    assert(
      eval("{def f1(x: Int) = x+1; def f1[A](x: A) = x;  f1(true)}") ==
         BoolValue(true))
    assert(
      eval("{def f2[A](x: A) = x; def f2(x: Int) = x+1; f2(true)}") ==
      BoolValue(true))
    // assert(
    //   eval("{def f2[A](x: A) = x; def f2(x: Int) = x+1; (f2(3), f2(true))}") ==
    //   TupleValue(IntValue(3), BoolValue(true))) // 3, true
    assert(
      eval("{def sum(xs: List[Int]): Int = "+
        "  if(isEmpty xs) 0 else head xs + sum(tail(xs)) \n" +
        "def sum(xs: List[Float]): Float = "+
        "  if(isEmpty xs) 0.0 else head xs + sum(tail(xs)) \n" +
        "val s = sum: List[Int] => Int; "+
        "(sum[2.4,4.6], s [2,4]) }"
      ) == TupleValue(FloatValue(7.0F), IntValue(6)) )
    // Concrete type parameters
    assert(eval("{def f[A](x:A) = x; f[Int](3)}") == IntValue(3))
    assert(eval("{def f[A <: Eq](x:Int) = 3; def f[A](x:Float) = 4.0; "+
      "val f1 = f[Int => Int]; f1(3.0)}") == FloatValue(4.0F))
    assert(eval("{ def f[A](x:Float) = 4.0; def f[A <: Eq](x:Int) = 3;"+
      "val f1 = f[Int => Int]; f1(3.0)}") == FloatValue(4.0F))
    // Following fails with current implementation.
    // eval("{def f[A](x:Int) = 3; def f[A](x:Int) = 4.0; val f1 = f[Int]; f1(3)}")
    // Now with explicit types
    assert(eval("{def f[A](x:Float) = 4.0; "+
      "val f1 = f[Int => Int]: Float => Float; f1(3.0)}") == FloatValue(4.0F))
    assert(eval("{def f[A <: Eq](x:Int) = 3; def f[A](x:Float) = 4.0; "+
      "val f1 = f[Int => Int]: Float => Float; f1(3.0)}") == FloatValue(4.0F))
    assert(eval("{def f[A](x:Int) = 3; def f[A](x:Float) = 4.0; "+
      "val f1 = f[Int]: Float => Float; f1(3.0)}") == FloatValue(4.0F))
    // First choice taken in following -- no longer
    // assert(eval("{def f[A](x:A) = 3.0; def f[A](x:Float) = 4.0; "+
    //   "val f1 = f[Float]: Float => Float; f1(3.0)}") == FloatValue(3.0F))
    // assert(eval("{def f[A](x:Float) = 4.0; def f[A](x:A) = 3.0; "+
    //   "val f1 = f[Float]: Float => Float; f1(3.0)}") == FloatValue(4.0F))
    // Overloaded function application
    assert(eval("{def f[A](x: A) = x; def f[A,B](x: Int) = 3; f[Int](4)}") ==
      IntValue(4))
    assert(eval("{def f[A](x: A) = x; def f[A,B](x: Int) = 3; f[Int,Int](4)}") ==
      IntValue(3))
    assert(eval("{def f[A](x: A) = x; def f[A](x: Float) = 3; f[Int](4)}") ==
      IntValue(4))
    assert(eval("{def f[A](x: A) = x; def f[A](x: Float) = 3; f[Int](4.0)}") ==
      IntValue(3))
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

  /** Tests on the script evaluationTest.dir, which uses haskell.dir. */
  private def scriptTests() = {
    def mkList(xs: Int*) = ListValue(xs.toList.map(IntValue(_)))
    val Height = 100; val Width = 26
    val model = new Model(Height,Width); model.setView(TestingView)
    val env = model.getEnv; val isCalculated = env.isCalculated _
    //model.loadScript("haskell.dir", null)
//println("=========")
    model.loadScript("evaluationTest.dir", null)

    assert(env("b1") == BoolValue(false) && env("b2") == BoolValue(true))
    assert(env("b2a")  == BoolValue(true))
    assert(env("b3") == BoolValue(true) && env("b4") == BoolValue(false))
    assert(env("b5") == BoolValue(false)) // lazy evaluation of &&
    assert(env("xs1") == mkList(0,1,2,3) && env("xs2") == mkList(3,3,3,0))
    assert(env("xs3") == mkList(0,3,3,3) && env("xs4") == mkList(3,2,1,0))
    assert(env("x1") == IntValue(32) && env("x2") == FloatValue(32.0F))
    assert(env("xs5") == mkList(2,3,4,5) && env("xs6") == mkList(6,7,8,9))
    assert(env("xs7") == mkList(2,4) && env("xs8") == mkList(5,6))
    assert(env("x3") == IntValue(7))
    assert(env("xs9") == mkList(1,2,3))
    assert(env("xs10") == ListValue(List(FloatValue(3.4F))))
    assert(env("st1") == StringValue("a"))
    assert(env("r1") == RowValue(3))
    assert(env("c1") == ColumnValue(4))
    //assert(env("b6") == BoolValue(true))
    //println(env("b7"))
  }

  /** Tests on val declarations using tuples. */
  def tests5() = {
    assert(eval("{ val (x,y) = (3,7); x+y }") == IntValue(10))
    assert(eval("{ val ((x1,x2,x3), y) = ((1,3,5), 7); x1+x2+x3+y }") == 
      IntValue(16))
  }

  /** Tests on String operations. */
  def tests6() = {
    //println(eval("\"Hello\"+toString(3)"))
    assert(eval("\"Hello\"+toString(3)") == StringValue("Hello3"))
    assert(eval("toString([1,2,3])") == StringValue("[1, 2, 3]"))
    assert(eval("toString((2.5,true,#A,#4))") == 
      StringValue("(2.5, true, #A, #4)"))
    assert(eval("\"Hello\"+\"world\"+3") == StringValue("Helloworld3"))
    assert(eval("toString(3.5)+(true,[1,2])") == 
      StringValue("3.5(true, [1, 2])"))
    assert(eval("\"Hello\" <= \"World\"") == BoolValue(true))
    assert(eval("\"Hello\" >= \"World\"") == BoolValue(false))
    assert(eval("\"Hello\" >= \"Hello\"") == BoolValue(true))
  }

  /** Tests on Num type class. */
  def tests7() = {
    // println("***")
    assert(eval("{def zero[A <: Num](): A = 0; zero[Int]()}") == IntValue(0))
    assert(eval("{def zero[A <: Num](): A = 0; zero[Float]()}") ==
      FloatValue(0.0F))
    assert(eval("{def zero[A <: Num]() = 0: A; zero[Float]()}") == 
      FloatValue(0.0F))

    // println(eval("{def f[A <: Num](x: A):A = 0:A; f(2.3)}"))
//sys.exit()
    // FIXME: should be FloatValue(0.0)

    val sumS = "def sum[A <: Num](xs: List[A]): A = "+
      "if(isEmpty xs) 0 else head xs + sum[A](tail xs)"
    assert(eval(s"{$sumS ; sum[Int] [1,2]}") == IntValue(3))
    assert(eval(s"{$sumS ; sum[Float] [1.3,2.7]}") == FloatValue(4.0F))
    val sumS1 = "def sum[A <: Num](xs: List[A]): A = "+
      "if(isEmpty xs) 0 else head xs + sum(tail xs)"
    assert(eval(s"{$sumS1 ; sum[1,2]}") == IntValue(3))
    //println(eval(s"{$sumS1 ; sum[Float] [1.3]}")) // FIXME
  }


  def main(args: Array[String]) = {
    // println(eval( 
    //   "{def zip[A,B](xs: List[A])(ys: List[B]): List[(A,B)] = "+
    //     "  if(isEmpty xs || isEmpty ys) [] "+
    //     "  else (head xs, head ys) :: zip (tail xs) (tail ys)\n" +
    //     "zip (2 to 5) (6 to 12)}"))

    // println(eval( 
    //   "{def zip[A,B](xs: List[A])(ys: List[B]): List[A] = zip xs ys\n" +
    //     "zip [2] [6]}"))

    // println(eval( 
    //   "{def zip[A,B](xs: List[A])(ys: List[B]): List[(A,B)] = "+
    //     "  if(isEmpty xs || isEmpty ys) [] "+
    //     "  else (head xs, head ys) :: zip (tail xs) (tail ys)\n" +
    //     "zip [2] [6]")) // (2 to 5) (6 to 12)}"))


//    println(eval("{def apply[A](f: List[Int] => A) = f [3]; apply isEmpty}"))
//sys.exit()
    // println(eval("{def apply[AA,B](f: AA => B, x: AA) = f(x); apply(isEmpty, [3])}"))
//    println(eval("{def apply[AA,B](f: AA => B)(x: AA) = f(x); def g(x: Int) = x == x; apply g 4}")) -- this works

    // println(eval( -- this works
    //   "{def append[AA](xs: List[AA])(ys: List[AA]): List[AA] = if(isEmpty xs) ys else append (tail xs) ys\n" +
    //     "def concat[A](xs: List[List[A]]): List[A] = "+
    //     "  if(isEmpty xs) [] else append (head xs)  (concat(tail xs)) \n"+
    //     "concat [[3]]}"))

/*
    println(eval(
      "{def loop[AA](xs: List[AA])(ys: List[AA]): List[AA] = loop xs ys\n" +
        "def two[A](f: A => A => A, e: A): A = f e e\n" +
        "two (loop, [3])}"))
 */

/*
    assert(eval(
      "{def apply[AA,B](f: AA => B)(x: AA) = f(x); "+
        "def g[A <: Eq](x: A) = x == x; apply g 4}") == BoolValue(true))
    assert(eval(
      "{def apply[AA,B](f: AA => B)(x: AA) = f(x); apply isEmpty [3]}") == 
      BoolValue(false))
    assert(eval("{def after[A,B,C](f: B => C)(g: A => B)(x: A): C = f(g x)\n"+
      "val nonEmpty = after not isEmpty; nonEmpty [3]}") == BoolValue(true))
 */
    println("===EvaluationTest===")
    tests1() // basic expressions
    tests2() // blocks, if statements, list expressions
    tests3() // functions
    tests4() 
 
    scriptTests()
    tests5() // val declarations with tuples
    tests6() // toString, + over Strings
    tests7()
  }

}
