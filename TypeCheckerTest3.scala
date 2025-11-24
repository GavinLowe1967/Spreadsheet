package spreadsheet

import TypeT._
import TypeCheckerTest0._

object TypeCheckerTest3{
  def forLoopTests() = {
    // Straightforward for loop
    assertOk(tcpss("for(r <- [#1,#2,#3]) Cell(#A, r) = 3"))
    // "Expected List, found Int"
    assertFail(tcpss("for(x <- 23) #A1 = x"))
    // "Expected cell type, found Row in r"
    assertFail(tcpss("for(r <- [#1,#2,#3]) #A1 = r"))
    // "Expected Row, found Int"
    assertFail(tcpss("for(r <- [1,2,3]) Cell(#A,r) = 3"))
    // Nested loop
    assertOk(tcpss("for(r <- [#1,#2,#3]; c <- [#A,#B,#C]) Cell(c, r) = 3"))
    // Nested loop with same name.  In an earlier version, the first r
    // escaped!
    tcpss("for(r <- [1,2,3]; r <- [#1,#2,#3]) Cell(#A, r) = 3") match{ 
      case Ok(te) => assert(te.get("r").isEmpty) }
    // For loop with block for body.
    assertOk(tcpss("for(r <- [#1,#2,#3]){ val x = 3; Cell(#A, r) = x }"))

    // Now with guards.
    assertOk(tcpss("for(r <- [#1,#2,#3]; if r != #2) Cell(#A, r) = 3"))
    assertOk(tcpss("for(if false; r <- [#1,#2,#3]; if r != #2) Cell(#A, r) = 3"))
    // "Expected Boolean, found Int"
    assertFail(tcpss("for(r <- [#1,#2,#3]; if 13) Cell(#A, r) = 3"))
  }

  def overloadingTests() = {
    val doubleS = 
      "def double(x: Int): Int = 2*x; def double(x: Float): Float = 2.0*x\n"
    val sumS = // ok, these definitions are wrong
      "def sum(xs: List[Int]): Int = 0; def sum(xs: List[Float]): Float = 0.0"
    val applyS = "def apply[A,B](f: A => B, x: A): B = f(x)\n"
//printErrors = true
    assertFail(tcpss("val x = 3; val x = 5"))
    assertFail(tcpss("val x = 3; def x(y: Int): Int = y+1"))
    assertFail(tcpss("def f(x: Int): Int = x+1; def f[A](x: A): A = x"))
    // "Forward reference to name f"
    assertFail(tcpss("val x = f(3)\n val f = 5"))
    assertFail(tcpss("val x = f(3)"))

    // Application of double
    tcpss(doubleS+"val y = double(2); val z = double(3.4)") match{
      case Ok(te) => 
        assert(te.get("double").get == List(
          FunctionType(List(), List(IntType), IntType),
          FunctionType(List(), List(FloatType), FloatType)
        ) )
        assert(te("y") == IntType && te("z") == FloatType) 
    }
    // "Overloaded function application with types (Int) => Int, (Float) =>
    // Float can't be applied to argument of type Boolean"
    assertFail(tcpss(doubleS+"val x = double(true)"))

    // Application of sum
    tcpss(sumS+"\nval x = sum([2,3,4]); val y = sum([2.4,3.4])") match{ 
      case Ok(te) =>
        assert(te.get("sum").get == List(
          FunctionType(List(), List(ListType(IntType)), IntType),
          FunctionType(List(), List(ListType(FloatType)), FloatType)
        ) )
        assert(te("x") == IntType && te("y") == FloatType)
    }
    assertFail(tcpss(sumS+"; val x = sum([true])"))
    assertFail(tcpss(sumS+"; val x = sum([])"))
    assertFail(tcpss(sumS+"; val x = sum(5)"))
    tcpss(sumS+"; val x = sum([]: List[Int])") match{ case Ok(te) =>
      assert(te("x") == IntType) }

    // Overloaded name with type.
    val script = doubleS + applyS +
        "val x = apply(double: Float => Float, 3.0); "+
        "val y = apply(double: Int => Int, 3)"
    tcpss(script) match{ case Ok(te) => 
      assert(te("x") == FloatType && te("y") == IntType) }
    assertFail(tcpss(
      doubleS+applyS+"val x = apply(double: Boolean => Boolean, true)"))

// printErrors = false

  }

  def curryingTests() = {
    val addS = 
      "def add(x:Int): Int => Int = { def addX(y: Int): Int = x+y; addX }\n"
    val constS = 
      "def const[A,B](x: A): B => A = { def constX(y: B): A = x; constX }\n"
    val constSC = 
      "def const[A,B](x: A): B => A = { def constX[C](y: C): A = x; constX }\n"
    val sndS = "def snd[A,B](x: A): B => B = { def id(y: B): B = y; id}\n"
    val applyS = 
      "def apply[A,B](f: A => B): A => B = {def ff(x: A): B = f(x); ff }\n"

    tcpss(addS+"val add3 = add(3)") match{ case Ok(te) =>
      assert(te("add") == FunctionType(
        List(), List(IntType), FunctionType(List(),List(IntType),IntType) ))
      assert(te("add3") == FunctionType(List(), List(IntType), IntType))
    }

    val constS1 = constS+"val const3 = const(4); val constTrue = const(true)\n"
    tcpss(constS1) match{ case Ok(te) => 
      assert(te("const") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(TypeParam("A")),
        FunctionType(List(), List(TypeParam("B")), TypeParam("A")) ))
      assert(te("const3") == FunctionType( // forall B, B => Int
        List(("B",AnyTypeConstraint)), List(TypeParam("B")), IntType ) )
      assert(te("constTrue") == FunctionType( // forall B, B => Bool
        List(("B",AnyTypeConstraint)), List(TypeParam("B")), BoolType ) )
    }
// FIXME: get following to work
    println(tcpss(constSC)); println()

    tcpss(sndS+"val sx = snd(4); val x = sx(3); val y = sx(4.0)") match{ 
      case Ok(se) =>
        assert(se("snd") == FunctionType(
          List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
          List(TypeParam("A")),
          FunctionType(List(),List(TypeParam("B")),TypeParam("B")) ))
        assert(se("sx") == FunctionType(
          List(("B",AnyTypeConstraint)), List(TypeParam("B")), TypeParam("B") ))
        assert(se("x") == IntType && se("y") == FloatType)
    }

    val applyS1 = 
      applyS+constS+"val c = apply(const); val c3 = c(3); val x = c3(5)"
    // val y = c3(true)
    //println(tcpss(applyS1))
    tcpss(applyS1) match{ case Ok(te) =>
      assert(te("apply") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B"))),
        FunctionType(List(), List(TypeParam("A")), TypeParam("B")) ))
      println("c : "+te("c")) 
        // case FunctionType(List(), List(TypeVar(ta)), 
        //     FunctionType(List(), List(TypeVar(tb)), TypeVar(ta)) ) =>
// FIXME: I think those should be TypeVars
      println("c3: "+
te("c3"))
      assert(te("x") == IntType)
    }

  }

}
