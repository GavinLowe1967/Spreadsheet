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
}
