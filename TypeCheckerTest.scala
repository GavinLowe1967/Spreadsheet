package spreadsheet

import scala.collection.immutable.{Map,HashMap}

/** Tests on the type checker. */
object TypeCheckerTest{
  import TypeT._
  import TypeChecker._; import TypeEnv._
  import TypeChecker.TestHooks._
  import Parser.parseAll; import ExpParser.expr
  import StatementParser.{statement,statements}
  import NameExp.Name // Names of identifiers (Strings)

  var printErrors = false // Should error messages be printed?

  // val typeCheck = TestHooks.typeCheck
  // val typeCheckStmtList = TestHooks.typeCheckStmtList
  
  /* If printErrors, print error message. */
  def maybePrintError[A](x: Reply[A]) =
    if(printErrors) x match{ case FailureR(msg) => println(msg); case _ => {} }
  
  def assertFail[A](r: Reply[A]) = assert(r.isInstanceOf[FailureR], r)
  
  /* Assert that r is an Ok for type t. */
  def assertEq(r: Reply[TypeCheckRes], t: TypeT) =
    assert(r.isInstanceOf[Ok[TypeCheckRes]] &&
      r.asInstanceOf[Ok[TypeCheckRes]].x._2 == t, 
      s"Expected $t, found "+r.asInstanceOf[Ok[TypeCheckRes]].x._2)
  
  /* Get new type environment. */
  def newEnv: TypeEnv = TypeEnv() // new TypeEnv(new NameMap, new Constraints)
  
  /* Parse and typecheck expression given by st. */
  def tcp(st: String, env: TypeEnv = newEnv) = {
    val e = parseAll(expr, st); val res = typeCheck(env, e)
    maybePrintError(res); res
  }
    
  /* Parse and typecheck list of statements given by st. */
  def tcpss(st: String, env: TypeEnv = newEnv) = {
    val stmt = parseAll(statements, st); val res = typeCheckStmtList(env, stmt)
    maybePrintError(res); res
  }

  // ==================================================================

  /** Tests on basic expressions. */
  def expTests() = {
    assertEq(tcp("2"), IntType); assertEq(tcp("2+3"), IntType)
    assertFail(tcp("2+f+5"))
    assertFail(tcp("2+false+5")); assertFail(tcp("true+4"))
    assertEq(tcp("2 == 3"), BoolType); assertEq(tcp("2 <= 3"), BoolType)
    assertFail(tcp("true == 3")); assertFail(tcp("2 != false"))
    assertFail(tcp("true <= 3")); assertFail(tcp("2 <= false"))
    assertFail(tcp("2 && false")); assertEq(tcp("true || false"), BoolType)
    // assertFail(tc(BinOp(StringExp("X"), "!=", StringExp("Y"))))
    //                                            TODO: need parser for Strings

    // "if" expressions
    assertEq(tcp("if(2 == 3) 4 else 5"), IntType)
    assertFail(tcp("if(2) 4 else 5"))
    assertFail(tcp("if(2 == 3) 4 else false"))
    assertFail(tcp("if(2 == 3) 2+true else 4"))

    // Lists
    assertFail(tcp("[1,2] == 3"))
    assertFail(tcp("[true, 4]"))
  }

  /** Tests on single declarations and function applications. */
  def singleDecTests() = {
    // Value declarations
    val Ok(te) = tcpss("val four = 4")
    assertEq(tcp("four", te), IntType); assertEq(tcp("2+four", te), IntType)
    assertEq(tcp("5 == four", te), BoolType)
    assertFail(tcpss("val x = 2+false"))

    //Function declarations
    // println(tcpss("def f(x: Int): Int = x+1", te))
    val Ok(te1) = tcpss("def f(x: Int): Int = x+1", te)
    assertEq(tcp("f", te1), FunctionType(List(IntType),IntType))
    val Ok(te2) = tcpss("def g(f: Boolean, x: Int): Int = if(f) x else 4", te1)
    assertEq(tcp("f", te2), FunctionType(List(IntType), IntType))
    assertEq(tcp("g", te2), FunctionType(List(BoolType,IntType), IntType))
    assertFail(tcpss("def f(x: Int): Int = if(x) 3 else 2"))
    assertFail(tcpss("def f(b: Boolean): Boolean = if(b)  3 else 2"))
    val Ok(te3) = 
      tcpss("def fact(n: Int): Int = if(n <= 0) 1 else n * fact(n-1)")
    assertEq(tcp("fact", te3), FunctionType(List(IntType), IntType))

    // Function applications
    assertEq(tcp("f(3)", te2), IntType)
    assertEq(tcp("g(true, 4)", te2), IntType)
    assertEq(tcp("fact(4)", te3), IntType)
    assertFail(tcp("f(true)", te2))
    assertFail(tcp("g(true, false)", te2)); assertFail(tcp("g(3, 4)", te2))
    assertFail(tcp("f(3,5)", te2)); assertFail(tcp("g(true)", te2))
    assertFail(tcp("four(4)", te2))
  }

  /** Tests on scripts and blocks. */
  def scriptTests() = {
    val script = 
      List(
        "def f(x: Int): Int = h(x+1,true)",
        "def g(f: Boolean, x: Int): Int = if(f) x else 4",
        "def fact(n: Int): Int = if(n <= 0) 1 else n * fact(n-1)",
        "def h(y: Int, b: Boolean): Int = if(b) y else f(y+1)",
        "val four = 4"
      ).mkString("\n")
    val Ok(te) = tcpss(script)
    assert(te("f") == FunctionType(List(IntType), IntType))
    assert(te("g") == FunctionType(List(BoolType,IntType), IntType))
    assert(te("fact") == FunctionType(List(IntType), IntType))
    assert(te("h") == FunctionType(List(IntType,BoolType), IntType))
    assert(te("four") == IntType)
    val faultyScript = script+"; def ff(x: Int): Int = if(x) 3 else 2"
    assertFail(tcpss(faultyScript))
    // Functions aren't equality types
    val script2 = "def f(x: Int): Int = x+1; val x = f == f"
    assertFail(tcpss(script2))

    // Tests on block expressions
    // printErrors = true
    assertFail(tcp("{ val x = three; x + 1 }"))
    assertFail(tcp("{ val x = 3; x + false }"))
    assertEq(tcp("{"+script+"; fact(4) }"), IntType)
    assertFail(tcp("{"+script+"; fact(true) }"))
    assertFail(tcp("{"+faultyScript+"; fact(4) }"))
  }

  /** Tests on cell expressions. */
  def cellTests() = {
    val script = "val x = Cell(#B, #2); def f(y: Int): Int = 3"
    tcpss(script) match{ case Ok(te) => 
      // x is just a Cell here
      te("x") match{ case TypeVar(t) => assert(te(t) == MemberOf(CellTypes)) }
      tcp("if(x) 3 else 4", te) match{ case Ok((te2, IntType)) =>
        assert(te2("x") == BoolType) // But now it's a Bool
        assertFail(tcp("f(x)", te2))
      }
    }
    val script2 = "val x = Cell(#B, #2); val ys = [x, 1]"
    tcpss(script2) match{ case Ok(te) => 
      assert(te("x") == IntType && te("ys") == ListType(IntType))
    }
    val script3 = "val x = Cell(#B, #2); val ys = [1, x]"
    tcpss(script3) match{ case Ok(te) => 
      assert(te("x") == IntType && te("ys") == ListType(IntType))
    }
    val script4 = "val x = Cell(#B, #2); val y = #A1; val ys = [y, x]"
    tcpss(script4) match{ case Ok(te) => 
      te("x") match{ case TypeVar(t) =>
        assert(te("y") == TypeVar(t) && te("ys") == ListType(TypeVar(t)) &&
          te(t) == MemberOf(CellTypes))
        tcp("[y, 2]", te) match{ case Ok((te2, ListType(IntType))) => 
          assert(te2("x") == IntType) // Now it's an Int
        }
      }
    }
    val script5 = "val x = #C3; def f(y: Int): Int = 3; val z = f(x)"
    tcpss(script5) match{ case Ok(te) => 
      assert(te("x") == IntType && te("z") == IntType)
    }
    val script6 = "val x = #C3; val y = #A2; "+
        "def f(y: Int, b: Boolean): Int = 3; val z = f(x,y)"
    tcpss(script6) match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == BoolType && te("z") == IntType)
    }
    val script7 =
      "val x = #C3; def f(y: Int, b: Boolean): Int = 3; val z = f(x,x)"
    assertFail(tcpss(script7))

    assertEq(tcp("if(2+2 == 4) 3 else #A2"), IntType)
    assertEq(tcp("if(2+2 == 4) #B3 else false"), BoolType)
    tcp("if(2+2 == 4) #B3 else #A5") match{ case Ok((_, TypeVar(_))) => {} }

    val script8 = "val x = #C3; val y = x; def f(x: Int): Int = if(y) x else 3"
    tcpss(script8) match{ case Ok(te) => 
      assert(te("x") == BoolType && te("y") == BoolType && 
        te("f") == FunctionType(List(IntType), IntType))
    }
    val script9 = 
      "val x = #C3; val y = x; def f(x: Int): Int = if(x==y) x else 3"
    tcpss(script9) match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == IntType && 
        te("f") == FunctionType(List(IntType), IntType))
    }
    val script10 = 
      "val x = #C3; val y = x; val z = { val x = 3; y && false }"
    tcpss(script10) match{ case Ok(te) => 
      assert(te("x") == BoolType && te("y") == BoolType && te("z") == BoolType)
    }
    val script11 = "val x = #C3; val y = x; val z = { val x = 3; y == 5 }"
    tcpss(script11) match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == IntType && te("z") == BoolType)
    }
    val script12 = "val x = #C3; def f(y: Int): Int = x"
    tcpss(script12) match{ case Ok(te) => assert(te("x") == IntType) }

    tcpss("val x = #C3; val y = x == 3") match{ case Ok(te) =>
      assert(te("x") == IntType && te("y") == BoolType)
    }
    tcpss("val x = #C3; val y = #C4; val z = x == y") match{ case Ok(te) =>
      te("x") match{ case TypeVar(tid) => 
        assert(te("y") == te("x") && te(tid) == MemberOf(CellTypes) &&
          te("z") == BoolType)
      }
    }
    val script13 = 
      "val x = #C3; val y = #C4; val w = if(x) y+3 else 4; val z = x == y"
    assertFail(tcpss(script13))
    // println(tcp("3 + Cell(#B, #1) + Cell(#B, #2)"))
    // println(tcp("3 + Cell(#B, #1)"))
    // println(tcpss("val x = Cell(#B, #1); val xx = x; val y = 3+xx"))
  }

  /** Tests writing to cells. */
  def cellWriteTests() = {
    tcpss("#A3 = 5") match{ case Ok(_) => {} }
    assertFail(tcpss("def f(y: Int): Int = 3; #A3 = f"))
    assertFail(tcpss("#A3 = true+5"))
    tcpss("val y = #B4; #A3 = y") match{ case Ok(te) => te("y") match{
      case TypeVar(tid) => assert(te(tid) == MemberOf(CellTypes))
    }}
    tcpss("val y = #B4+#B5; #A3 = y") match{ case Ok(te) => te("y") match{
      case TypeVar(tid) => assert(te(tid) == MemberOf(NumTypes))
    }}
  }


  // =======================================================

  def main(args: Array[String]) = {
    // printErrors = true
    expTests()
    singleDecTests()
    scriptTests()
    //printErrors = true
    cellTests()
    cellWriteTests()

    printErrors = true


    // lists
    assertEq(tcp("[1,2,3]"), ListType(IntType))
    assertFail(tcp("[1, 3, false, 2, true]"))
    tcp("[]") match{ case Ok((te, ListType(TypeVar(t)))) => 
      assert(te(t) == AnyTypeConstraint) }
    assertEq(tcp("3 :: [ ]"), ListType(IntType))
    tcpss("val xs = 3 :: []") match{ case Ok(te) => 
      assert(te("xs") == ListType(IntType)) }
    tcpss("val xs = true :: []") match{ case Ok(te) => 
      assert(te("xs") == ListType(BoolType)) }
    // [] :: [] : ListType(ListType(a))
    tcpss("val xs = [] :: []") match{ case Ok(te) => 
      te("xs") match{ case ListType(ListType(TypeVar(t))) => 
        assert(te(t) == AnyTypeConstraint) } }
    tcpss("val xs = [[]]") match{ case Ok(te) => 
      te("xs") match{ case ListType(ListType(TypeVar(t))) => 
        assert(te(t) == AnyTypeConstraint) } }
    tcpss("val xs = 1 :: 2 :: []") match{ case Ok(te) => 
      assert(te("xs") == ListType(IntType)) }
    assertFail(tcp("1:: [true]"))
    assertFail(tcpss("val xs = [1]; val ys = true::xs"))
    tcpss("val xs = []; val ys = 1::xs") match{ case Ok(te) => 
      assert(te("xs") == ListType(IntType) && te("ys") == ListType(IntType)) }
    assertEq(tcp("[1] == [2]"), BoolType)
    // Can't do following, yet: need polymorphic type for tail. 
    // println(tcp("tail([1]) == tail([false])"))                 FIXME
    tcpss("val xs = [#A1, #A2]") match{ case Ok(te) => 
      te("xs") match{ case ListType(TypeVar(tid)) => 
        assert(te(tid) == MemberOf(CellTypes)) }}
    tcpss("val x = #A1; val xs = [x, #A2]; val y = x+3") match{ case Ok(te) => 
      assert(te("xs") == ListType(IntType)) }
    tcpss("val x = #D0; val y = #D3; val eq = [x] == [y]") match{ case Ok(te) => 
      assert(te("eq") == BoolType)
      te("x") match { case TypeVar(tid) => 
        assert(te("y") == TypeVar(tid) && te(tid) == MemberOf(CellTypes))
      }}
    tcpss("val xs = [#D0]; val y = #D3; val eq = xs == [y]") match{ 
      case Ok(te) =>
        assert(te("eq") == BoolType)
        te("y") match { case TypeVar(tid) =>
          assert(te("xs") == ListType(TypeVar(tid)) && 
            te(tid) == MemberOf(CellTypes))
        }}

    println("Done")
  }

}
