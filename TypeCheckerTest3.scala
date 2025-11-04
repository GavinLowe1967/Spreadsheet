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
    assertFail(tcpss("val x = 3; val x = 5")) // Needs line numbers
    assertFail(tcpss("val x = 3; def x(y: Int): Int = y+1"))  // Needs line numbers
    assertFail(tcpss("def f(x: Int): Int = x+1; def f[A](x: A): A = x")) // Needsl line number
    // "Forward reference to name f"
    assertFail(tcpss("val x = f(3)\n val f = 5"))
    assertFail(tcpss("val x = f(3)"))


    val doubleS = 
      "def double(x: Int): Int = 2*x; def double(x: Float): Float = 2.0*x"
    tcpss(doubleS) match{ case Ok(te) => 
      assert(te.get("double").get == List(
        FunctionType(List(), List(IntType), IntType), 
        FunctionType(List(), List(FloatType), FloatType)
      ) ) 
    }
  }
}
