package spreadsheet

import TypeT._
import TypeCheckerTest0._
import TypeCheckerTest1._
import TypeChecker.TestHooks._

/** Tests on the type checker. */
object TypeCheckerTest{


  // ==================================================================

  // The expected type of the identity function
  val idFunctionType = FunctionType(
      List(("A",AnyTypeConstraint)), List(TypeParam("A")), TypeParam("A") )

  /** Tests on polymorphic functions. */
  def polyTests() = {
    tcpss("def add[A](x: Int, y: Int) : Int = x+y") match{ case Ok(te) => 
      assert(te("add") == FunctionType(
        List(("A",AnyTypeConstraint)), List(IntType, IntType), IntType ) )
    }
    tcpss("def id[A](x: A) : A = x") match{ case Ok(te) =>
      assert(te("id") ==  idFunctionType)
    }
    tcpss("def apply[A, B](f: A => B, x: A) : B = f(x)") match{ case Ok(te) =>
      assert(te("apply") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B")), 
          TypeParam("A")),
        TypeParam("B") ) )
    }
    tcpss("def id[A](x: A) : A = x; val y = id(3)") match{ case Ok(te) =>
      assert(te("y") == IntType) 
    }
    val script = "def apply[A, B](f: A => B, x: A) : B = f(x);"+
      "def g(x: Int): Boolean = x > 2"
    tcpss(script+"; val b = apply(g, 4)") match{ case Ok(te) => 
      assert(te("b") == BoolType) }
    assertFail(tcpss("def f[A](x: A, y: A): A = x; val y = f(3, true)"))
    assertFail(tcpss(script+"; val c = apply(g, true)"))

    val script1 = "def f[A](x: A): A = { def g(y: A): A = y; g(x) }"
    tcpss(script1) match{ case Ok(te) => assert(te("f") == idFunctionType) }
    val script2 = "def f[A](x: A): A = { def g[A](y: A): A = y; g(x) }"
    tcpss(script2) match{ case Ok(te) => assert(te("f") == idFunctionType) }
    val script3 = "def f[A](x: A): A = { def g[B](y: B): B = y; g(x) }"
    tcpss(script3) match{ case Ok(te) =>  assert(te("f") == idFunctionType) }
    val script4 = "def f[A](x: A): A = { def g[B](y: B): A = y; x }" // g(x)
    assertFail(tcpss(script4))
    val script5 = "def f[A](x: A): A = { def g(y: Int): Int = y; g(x) }"
    assertFail(tcpss(script5))
    val script6 = "def f(x: Int): Int = { def g[A](y: A): A = y; g(x) }"
    tcpss(script6) match{ case Ok(te) => 
      assert(te("f") == FunctionType(List(), List(IntType), IntType))
    }
    assertFail(tcpss("def f[A](x: A): Int = x"))
    assertFail(tcpss("def f[A](x: A): Boolean = x > 3"))

    // Sanity checks on parameters
    assertFail(tcpss("def f[A](x: A, x: A): Int = 0")) // repeated params
    assertFail(tcpss("def f[A, A](x: Int): Int = 0")) // repeated tparams
    assertFail(tcpss("def f(x: A): Int = 0")) // missing tparam

    tcpss("def f[A](x: A): Int = { def g[A](y: A) : A = y; g(2) }") match{
      case Ok(te) => 
        assert(te("f") == FunctionType(
          List(("A",AnyTypeConstraint)), List(TypeParam("A")), IntType ) )
    }
    // Below, the type parameter "A" of g is different from the type parameter
    // "A" of f.  The former gets instantiated with Boolean, and the latter
    // with Int.
    val script7 = 
      "def f[A](x: A): A = { \n"+
      "  def g[A](y: A) : A = y; def h(z: A): A = z; if(g(true)) h(x) else x \n"+
      "}; val w = f(3)"
    tcpss(script7) match{ case Ok(te) => 
      assert(te("f") == idFunctionType); assert(te("w") == IntType)
    }
    assertFail(tcpss("def f[A](x: A): Boolean = x == x"))
  }

  // ==================================================================

  /** Tests using type constraints. */
  def typeConstraintTests() = {
    val script8 = 
      "def f[A <: Eq](x: A): Boolean = x == x; val y = f(3); val z = f(#B4: Int)"
    tcpss(script8) match{ case Ok(te) =>
      assert(te("f") == FunctionType(
        List(("A", EqTypeConstraint)), List(TypeParam("A")), BoolType ) )
      assert(te("y") == BoolType); assert(te("z") == BoolType)
    }
    val script9 =
      "def f[A <: Eq](x: A): Boolean = x == x; def g[A](y: A): A = y; "+
        "val z = f(g)"
    assertFail(tcpss(script9))
    assertFail(tcpss("def f[A](x: A): A = x+x"))
    tcpss("def f[A <: Num](x: A): A = x+x") match{ case Ok(te) =>
      assert(te("f") == FunctionType(
        List(("A", NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
    }
    assertFail(tcpss("def f[A <: Eq](x: A): A = x+x"))

    tcpss("def f[A <: Num](x: A): Boolean = x == x; val y = f(3)") match{
      case Ok(te) =>
        assert(te("f") == FunctionType(
          List(("A", NumTypeConstraint)), List(TypeParam("A")), BoolType) )
        assert(te("y") == BoolType)
    }
    assertFail(tcpss("def f[A <: Num](x: A): Boolean = x == x; val y = f(true)"))

    val script10 =
      "def f[A <: Eq](x: A): Boolean = g(x) == x; def g[B <: Num](x: B): B = x"
    assertFail(tcpss(script10))
    val script11 =
      "def f[A <: Num](x: A): Boolean = g(x) == x; def g[B](x: B): B = x"
    tcpss(script11) match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A", NumTypeConstraint)), List(TypeParam("A")), BoolType) )
    }
    assertFail(tcpss("def f[A <: Num](x: A): A = 3+x"))
    //   match{ case Ok(te) =>
    //   assert(te("f") == FunctionType(
    //     List(("A", NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
    // }
// FIXME: why does above fail, but following succeeds?
    tcpss("def f[A <: Num](x: A): A = x+3") match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A", NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
    }
    assertFail(tcpss("def f[A, B](x: A): B = x"))
    tcpss("val y = #B3:Int; def f(): Int = y") match{ case Ok(te) => 
      assert(te("f") == FunctionType(List(), List(), IntType))
      assert(te("y") == IntType)
    }
    tcpss("val y = #B3:Float; def f[A <: Num](): A = y") match{ case Ok(te) =>
      assert(te("y") == FloatType)
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    tcpss("val y = #B3: Int; def f[A <: Num](): A = y+3") match{ case Ok(te) =>
      assert(te("y") == IntType)
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    tcpss("val y = #B3: Int; def f[A <: Num](): A = 3+y") match{ case Ok(te) =>
      assert(te("y") == IntType)
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    tcpss("def f[A <: Num](): A = 3") match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    assertFail(tcpss("def f(x: A): A = #B3: Int"))
    assertFail(tcpss("def f(x: Float): Float = if(true) x else #B3: Int"))
    // Following two fail, because #B3 would need to hold value of type A, for
    // all A.

// FIXME: following should fail
    // println(tcpss("def f[A](x: A): A = if(true) x else #B3: Int"))
    // assertFail(tcpss("def f[A](x: A): A = if(true) x else #B3: Int"))
    // assertFail(tcpss("def f[A](x: A): A = #B3: Int"))

    // Below, x doesn't satisfy Num
    val script12 = "def f[A <: Eq](x: A): Boolean = g(x); "+
      "def g[B <: Num](y: B): Boolean = y == y"
    assertFail(tcpss(script12))
    val script2 = "def f[A <: Num](x: A): A = x; val y = f(3.0)"
    tcpss(script2) match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
      assert(te("y") == FloatType)
    }
  }

  // =======================================================

  /* Some definitions of functions for use in tests. */
  val double = "def double(y: Int) : Int = 2*y\n"
  val apply = "def apply[A,B](f: A => B, x: A): B = f(x)\n"
  val map = "def map[A, B](f: A => B, xs: List[A]): List[B] = "+
  "  if(isEmpty(xs)) [] else f(head(xs)) :: map(f, tail(xs))\n"
  val applyToThree = "def applyToThree[A <: Num, B](f: A => B): B = f(3)\n"
  val id = "def id[A](x: A): A = x\n"

  /** Tests using higher-order functions. */
  def higherOrderTests() = {
    // double
    tcpss(double+"val y = double(3)") match{ case Ok(te) =>
      assert(te("double") == FunctionType(List(), List(IntType), IntType))
      assert(te("y") == IntType)
    }
    assertFail(tcpss(double+"val y = double(2.2)"))

    // tcpss(double+"val y = double(3); val z = double(2.2)") match{ case Ok(te) =>
    //   assert(te("double") == FunctionType(
    //     List(("C",NumTypeConstraint)), List(TypeParam("C")), TypeParam("C")))
    //   assertNum(te, "y"); assert(te("z") == FloatType)
    // }
    // apply
    tcpss(apply+double+"val z = apply(double, 2)") match{ case Ok(te) => 
      assert(te("z") == IntType) 
      assert(te("apply") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B")), 
          TypeParam("A")),
        TypeParam("B") ))
    }
    // map
    tcpss(map) match{ case Ok(te) => 
      assert(te("map") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B")), 
          ListType(TypeParam("A"))),
        ListType(TypeParam("B"))) )
    }
    val doubleI = "def doubleI(x: Int): Int = x*2\n"
    val scriptDI = map+doubleI+"val xs = map(doubleI, [1,2])"
    tcpss(scriptDI) match{ case Ok(te) => assert(te("xs") == ListType(IntType)) }
    val scriptMD = doubleI+map+
      "def md(ys: List[Int]): List[Int] = map(doubleI, ys)"
    tcpss(scriptMD) match{ case Ok(te) => 
      assert(te("md") == FunctionType(
        List(), List(ListType(IntType)), ListType(IntType)) )
    }
    // map(double,_)
    val scriptMC1 = map+double+"val xs = map(double, [1,2])"
    tcpss(scriptMC1) match{ case Ok(te) => assertListInt(te, "xs") }
    // ==================== applyToThree
    val scriptA31 = applyToThree+double+"val y = applyToThree(double)"
    tcpss(scriptA31) match{ case Ok(te) => 
      assert(te("applyToThree") == FunctionType(
        List(("A",NumTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B"))),
        TypeParam("B")) )
      assert(te("y") == IntType)
    }
    val scriptA32 = applyToThree+id+"val y = applyToThree(id)"
    tcpss(scriptA32) match{ case Ok(te) => assertNum(te, "y")}

    // ===== apply returning Num
    val applyX = "def applyX[A, B <: Num ](f: A => B, x: A): B = f(x)\n"
    val three = "def three[A](x: A): Int = 3\n"
    val scriptAX3 = applyX+three+"val y = applyX(three, true)"
    tcpss(scriptAX3) match{ case Ok(te) => 
      assert(te("applyX") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",NumTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B")),
          TypeParam("A")),
        TypeParam("B")) )
      assert(te("three") == FunctionType(
        List(("A",AnyTypeConstraint)), List(TypeParam("A")), IntType) )
      assert(te("y") == IntType)
    }
    val scriptAXId = applyX+id+"val y = applyX(id, 4)"
    tcpss(scriptAXId) match{ case Ok(te) =>
      assert(te("id") == FunctionType(
        List(("A",AnyTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
      assertNum(te, "y")
    }
    // Following fails
    val scriptAXIdTrue = applyX+id+"val y = applyX(id, true)"
    assertFail(tcpss(scriptAXIdTrue))
    val script = "def applyToThreeF[A <: Num](f: A => A): A = f(3.0)\n"
    tcpss(script) match{ case Ok(te) =>
      assert(te("applyToThreeF") == FunctionType(
        List(("A",NumTypeConstraint)), 
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("A"))),
        TypeParam("A")) )
    }
  }



  // =======================================================

  def main(args: Array[String]) = {
    var doAll = true; var i = 0
    while(i < args.length) args(i) match{
      case "--restrict" => doAll = false; i += 1
    }


// // FIXME: following should fail// 
//     val doubleI = "def doubleI(x: Int): Int = x*2\n"
//     val scriptA33 = applyToThree+doubleI+"val y = applyToThree(doubleI)"
//     println(tcpss(scriptA33))

// if(doAll){
//   println("script")
//     val script = "def f[A <: Num](x: A): Int = { def g(y: Int): Int = 3; g(x) }"
//     assertFail(tcpss(script))
// }

    // TODO: more tests here

    if(doAll){
      println("===exptests===")
      // printErrors = true
      TypeCheckerTestExpr.expTests()
      println("===singleDecTests===")
      singleDecTests()
      println("===scriptTests===")
      scriptTests()
      println("===cellTests===")
      cellTests()
      println("===cellWriteTests===")
      cellWriteTests()
      println("===listTests===")
      listTests()
      println("===polyTests===")
      polyTests()
      println("===typeConstraintTests===")
      typeConstraintTests()
      println("===higherOrderTests===")
      higherOrderTests()
    }
    println("Done")
  }

}
