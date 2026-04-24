package spreadsheet

import TypeT._
import TypeCheckerTest0._

object TypeCheckerTest5{
  /** Tests involving tuples. */
  def tupleTests() = {
    println("===tupleTests===")
    tcpss("def pair(x: Int): (Int,Int) = (x,x)") match{ case Ok(te) =>
      assert(te("pair") == FunctionType(
        List(), List(IntType), TupleType(List(IntType, IntType))))
    }
    tcpss("def pair[A,B](x: A, y: B): (A,B) = (x,y)") match{ case Ok(te) =>
      assert(te("pair") == FunctionType(
        List(("A", AnyTypeConstraint), ("B", AnyTypeConstraint)),
        List(TypeParam("A"), TypeParam("B")),
        TupleType(List(TypeParam("A"), TypeParam("B"))) ))
    }
    // tcpss("val pair = (1,2.5); val x = get1From2(pair)") match{ case Ok(te) =>
    //   assert(te("pair") == TupleType(List(IntType,FloatType)))
    //   assert(te("x") == IntType)
    // }
    tcpss("val pair = (1,2.5); val x = get1(pair)") match{ case Ok(te) =>
      assert(te("pair") == TupleType(List(IntType,FloatType)))
      assert(te("x") == IntType)
    }
    // Expected (t379,t380,t381), found (Int,Float)
    // assertFail(tcpss("val pair = (1,2.5); val x = get1From3 pair"))
    assertFail(tcpss("val pair = (1,2.5); val x = get3 pair"))
    tcpss("def pair[A,B](x: A, y: B) = (x,y); val p = pair (2,3.5)") match{ 
      case Ok(te) => assert(te("p") == TupleType(List(IntType,FloatType)))
    }
    tcpss("def fst[A,B](p: (A,B)) = get1 p; val x = fst((2.3, 5))") match{
      case Ok(te) => assert(te("x") == FloatType) 
    }
    //println(tcpss("def id[A](x: A) = x; val y = id 3"))

  }

  /** Tests on function calls. */
  def callTests() = {
    tcpss("def f() = {#A3 = 3}; f()") match{ case Ok(te) => 
      assert(te("f") == FunctionType( List(), List(), UnitType))
    }
  }

  /** Tests on IF statements. */
  def ifStmtTests() = {
    assertOk(tcpss("IF (true) #A3 = 3 ELSE{ #A4 = 4 }"))
    assertOk(tcpss("IF (true) {#A3 = 3}"))
    // "Expected Boolean, found Int at line 1 in 3 in IF..."
    assertFail(tcpss("IF(3) #A3 = 3"))
    assertFail(tcpss("IF(true) #A3 = 4+true ELSE{} "))
    assertFail(tcpss("IF(false) #A3 = 4 ELSE #A4 = 6+true"))
  }

  /** Tests on operation declarations. */
  def operationTests() = {
    assertOk(tcpss("operation f() = #A3 = 3"))
    assertOk(tcpss("operation f() = #A3 = 3 \n f()"))
    assertFail(tcpss("operation f() = #A3 = 2+true")) 
  }


}
