package spreadsheet

import TypeT._
import TypeCheckerTest0._

/** Tests on the type checker.  Tests on scripts, but expluding polymorphism
  * and higher-order functions. */
object TypeCheckerTest1{

  /** Tests on single declarations and function applications. */
  def singleDecTests() = {
    // Value declarations
    val Ok(te) = tcpss("val four = 4")
    assertEq(tcp("four", te), IntType); assertEq(tcp("2+four", te), IntType)
    assertEq(tcp("5 == four", te), BoolType)
    assertFail(tcpss("val x = 2+false"))
    assertFail(tcpss("val y = 3 + 4.5")) // match{ case Ok(te) => assert(te("y") == FloatType) }
    tcpss("val y = 3 + 4") match{ case Ok(te) => assert(te("y") == IntType) }

    //Function declarations
    val Ok(te1) = tcpss("def f(x: Int): Int = x+1", te)
    assertEq(tcp("f", te1), FunctionType(List(), List(IntType),IntType))
    val Ok(te2) = tcpss("def g(f: Boolean, x: Int): Int = if(f) x else 4", te1)
    assertEq(tcp("f", te2), FunctionType(List(), List(IntType), IntType))
    assertEq(tcp("g", te2), FunctionType(List(), List(BoolType,IntType), IntType))
    assertFail(tcpss("def f(x: Int): Int = if(x) 3 else 2"))
    assertFail(tcpss("def f(b: Boolean): Boolean = if(b)  3 else 2"))
    val Ok(te3) = 
      tcpss("def fact(n: Int): Int = if(n <= 0) 1 else n * fact(n-1)")
    assertEq(tcp("fact", te3), FunctionType(List(), List(IntType), IntType))

    // Function applications
    assertEq(tcp("f(3)", te2), IntType)
    assertEq(tcp("g(true, 4)", te2), IntType)
    assertEq(tcp("fact(4)", te3), IntType)
    assertFail(tcp("f(true)", te2))
    assertFail(tcp("g(true, false)", te2)); assertFail(tcp("g(3, 4)", te2))
    assertFail(tcp("f(3,5)", te2)); assertFail(tcp("g(true)", te2))
    assertFail(tcp("four(4)", te2))
  }

  // ==================================================================

  /** Tests on basic scripts and blocks. */
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
    assert(te("f") == FunctionType(List(), List(IntType), IntType))
    assert(te("g") == FunctionType(List(), List(BoolType,IntType), IntType))
    assert(te("fact") == FunctionType(List(), List(IntType), IntType))
    assert(te("h") == FunctionType(List(), List(IntType,BoolType), IntType))
    assert(te("four") == IntType)
    val faultyScript = script+"; def ff(x: Int): Int = if(x) 3 else 2"
    assertFail(tcpss(faultyScript))
    // Functions aren't equality types
    val script2 = "def f(x: Int): Int = x+1; val x = f == f"
    // println(tcpss(script2))
    assertFail(tcpss(script2))

    // Tests on block expressions
    assertEq(tcp("{"+script+"; fact(4) }"), IntType)
    assertFail(tcp("{"+script+"; fact(true) }"))
    assertFail(tcp("{"+faultyScript+"; fact(4) }"))
  }

  // ==================================================================

  /** Tests on cell expressions. */
  def cellTests() = {
    val script = "val x = Cell(#B, #2): Int; def f(y: Int): Int = 3"
    tcpss(script) match{ case Ok(te) => 
      // x is just a Cell here
      assert(te("x") == IntType)
      assertFail(tcp("if(x) 3 else 4", te))
//      te("x") match{ case TypeVar(t) => assert(te(t) == MemberOf(CellTypes)) }
      // tcp("if(x) 3 else 4", te) match{ case Ok((te2, TypeVar(tv))) =>
      //   assert(te2(tv) == NumTypeConstraint)
      //   assert(te2("x") == BoolType) // But now it's a Bool
      //   assertFail(tcp("f(x)", te2))
      // }
    }
    val script2 = "val x = #B2: Int; val ys = [x, 1]; val zs = [1, x]"
    tcpss(script2) match{ case Ok(te) => 
      assert(te("x") == IntType && te("ys") == ListType(IntType) &&
        te("zs") == ListType(IntType))
      // te("x") match{ case TypeVar(tv) =>
      //   assert(te(tv) == NumTypeConstraint)
      //   assert(te("ys") == ListType(TypeVar(tv)))
      // }
    }
    // val script3 = "val x = Cell(#B, #2); val ys = [1, x]"
    // tcpss(script3) match{ case Ok(te) => 
    //   te("x") match{ case TypeVar(tv) => 
    //     assert(te(tv) == MemberOf(TypeT.NumTypes)) 
    //     assert(te("ys") == ListType(TypeVar(tv)))
    //   }
    // }
    val script4 = "val x = #A4: Float; val y = #A1: Int"
    tcpss(script4) match{ case Ok(te) => 
      assert(te("x") == FloatType && te("y") == IntType)
      assertFail(tcpss("val ys = [y, x]", te))
      // te("x") match{ case TypeVar(t) =>
      //   assert(te("y") == TypeVar(t) && te("ys") == ListType(TypeVar(t)))
      //   assert(te(t) == MemberOf(CellTypes))
      //   tcp("[y, 2]", te) match{ case Ok((te2, ListType(TypeVar(tv)))) =>
      //     assert(te2(tv) == NumTypeConstraint)
      //     assertNum(te2, "x") // Now it's a NumType
      //   }
      //}
    }
    val script5 = "val x = #C3: Int; def f(y: Int): Int = 3; val z = f(x)"
    tcpss(script5) match{ case Ok(te) => 
      assert(te("x") == IntType && te("z") == IntType)
    }
    val script6 = "val x = #C6: Int; val y = #A2: Boolean; "+
        "def f(y: Int, b: Boolean): Int = 3; val z = f(x,y)"
    tcpss(script6) match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == BoolType && te("z") == IntType)
    }
    val script7 =
      "val x = #C7: Int; def f(y: Int, b: Boolean): Int = 3; val z = f(x,x)"
    assertFail(tcpss(script7)) 
    assertEq(tcp("if(2+2 == 4) 3 else #A2: Int"), IntType)
    assertEq(tcp("if(2+2 == 4) #B3: Boolean else false"), BoolType)
    assertEq(tcp("if(2+2 == 4) #B3: String else #A5: String"), StringType)
    // tcp("if(2+2 == 4) #B3 else #A5") match{ case Ok((_, TypeVar(_))) => {} }
    val script8 = 
      "val x = #C8: Boolean; val y = x; def f(x: Int): Int = if(y) x else 3"
    tcpss(script8) match{ case Ok(te) => 
      assert(te("x") == BoolType && te("y") == BoolType && 
        te("f") == FunctionType(List(), List(IntType), IntType))
    }
    val script9 = 
      "val x = #C9: Int; val y = x; def f(x: Int): Int = if(x==y) x else 3"
    tcpss(script9) match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == IntType && 
        te("f") == FunctionType(List(), List(IntType), IntType))
    }
    val script10 = 
      "val x = #C10: Boolean; val y = x; val z = { val x = 3; y && false }"
    tcpss(script10) match{ case Ok(te) => 
      assert(te("x") == BoolType && te("y") == BoolType && te("z") == BoolType)
    }
    val script11 = "val x = #C11: Int; val y = x; val z = { val x = 3; y == 5 }"
    tcpss(script11) match{ case Ok(te) => 
      assert(te("x") == IntType); assert(te("y") == IntType)
      assert(te("z") == BoolType)
    }
    val script12 = "val x = #C12: Int; def f(y: Int): Int = x"
    tcpss(script12) match{ case Ok(te) => assert(te("x") == IntType) }

    tcpss("val x = #C3: Int; val y = x == 3") match{ case Ok(te) =>
      assert(te("x") == IntType); assert(te("y") == BoolType)
    }
    tcpss("val x = #C3: Float; val y = #C4: Float; val z = x == y") match{ 
      case Ok(te) =>
        assert(te("x") == FloatType && te("y") == FloatType &&
          te("z") == BoolType)
      // te("x") match{ case TypeVar(tid) => 
      //   assert(te("y") == te("x") && te(tid) == MemberOf(CellTypes) &&
      //     te("z") == BoolType)
      //}
    }
    val script13 = 
      "val x = #C3: Boolean; val y = #C4: Int; val w = if(x) y+3 else 4; val z = x == y"
    assertFail(tcpss(script13))
    val script14 = "val x = #A4: Float; val x2 = #A5: Float; val y = x == x; "+
      "val z = x2+x2; val w = x == x2"
    tcpss(script14) match{ case Ok(te) => 
      assert(te("x") == FloatType && te("z") == FloatType)
      // te("x") match{ case TypeVar(tId) => 
      //   assert(te(tId) == NumTypeConstraint); 
      //   assert(te("x2") == TypeVar(tId)); assert(te("z") == TypeVar(tId))
      // }
      assert(te("w") == BoolType); assert(te("y") == BoolType)
    }
  }

  // ==================================================================

  /** Tests writing to cells. */
  def cellWriteTests() = {
    tcpss("#A3 = 5") match{ case Ok(_) => {} }
    //println("1")
    //println(tcpss("def f(y: Int): Int = 3; #A3 = f"))
    assertFail(tcpss("def f(y: Int): Int = 3; #A3 = f"))
    assertFail(tcpss("#A3 = true+5"))
    tcpss("val y = #B4: Float; #A3 = y") match{ case Ok(te) =>
      assert(te("y") == FloatType)
      // te("y") match{
      // case TypeVar(tid) => assert(te(tid) == MemberOf(CellTypes))
    }
    tcpss("val y = #B4:Float + #B5:Float; #A3 = y") match{ case Ok(te) =>
      assert(te("y") == FloatType)
      // te("y") match{
      // case TypeVar(tid) => assert(te(tid) == MemberOf(NumTypes))
    }
  }

 // ==================================================================

  /** Tests on lists. */
  def listTests() = {
    //assertListInt(tcpss("val xs = 3 :: []"))
    tcpss("val xs = 3 :: []") match{ case Ok(te) => assertListInt(te,"xs") }
    tcpss("val xs = 1 :: 2 :: []") match{ case Ok(te) => assertListInt(te,"xs") }
    assertFail(tcpss("val xs = [1]; val ys = true::xs"))  // IMPROVE error
    tcpss("val xs = []; val ys = 1::xs") match{ case Ok(te) => 
      assertListInt(te, "xs"); assertListInt(te, "ys") }
    tcpss("val x = #A1: Int; val xs = [x, #A2: Int]; val y = x+3") match{ 
      case Ok(te) => assertListInt(te, "xs") }
    tcpss("val x = #D0: Float; val y = #D3: Float; val eq = [x] == [y]") match{
      case Ok(te) => assert(te("eq") == BoolType); assert(te("x") == FloatType)
    }
     // te("x") match { case TypeVar(tid) => 
      //   assert(te("y") == TypeVar(tid) && te(tid) == MemberOf(CellTypes))
      // }}
    assertFail(tcpss("val x = #D0: Float; val y = #D3: Int; val eq = [x] == [y]"))
    tcpss("val xs = [#D0:Int]; val y = #D3:Int; val eq = xs == [y]") match{ 
      case Ok(te) =>
        assert(te("eq") == BoolType && te("xs") == ListType(IntType))
        // te("y") match { case TypeVar(tid) =>
        //   assert(te("xs") == ListType(TypeVar(tid)) && 
        //     te(tid) == MemberOf(CellTypes))
    }
    tcpss("val xs = [1,2,3]; val x = head(xs)") match{
      case Ok(te) => assertListInt(te, "xs"); assert(te("x") == IntType)
    }
    tcpss("val xs = [true, false]; val x = head(xs)") match{ case Ok(te) => 
      assert(te("xs") == ListType(BoolType) && te("x") == BoolType)
    }
    tcpss("val xs = [[1,2],[3]]; val x = head(xs)") match{ case Ok(te) => 
      assertListListInt(te, "xs"); assertListInt(te, "x")
    }
    tcpss("val xs = [[1,2],[3]]; val ys = tail(xs)") match{ case Ok(te) => 
      assertListListInt(te, "xs"); assertListListInt(te, "ys")
    }
  }

}
