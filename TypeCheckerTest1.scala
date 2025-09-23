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
    assertFail(tcpss("val y = 3 + 4.5"))
    tcpss("val y = 3 + 4") match{ case Ok(te) => assert(te("y") == IntType) }
    // The following succeeds, but gives an evaluation error.
    tcpss("val x = head([])") match{ case Ok(te) => te("x") match{
      case TypeVar(t) => assert(te(t) == AnyTypeConstraint)
    } }

    //Function declarations
    val Ok(te1) = tcpss("def f(x: Int): Int = x+1", te)
    assertEq(tcp("f", te1), FunctionType(List(), List(IntType), IntType))
    val Ok(te2) = tcpss("def g(f: Boolean, x: Int): Int = if(f) x else 4", te1)
    assertEq(tcp("f", te2), FunctionType(List(), List(IntType), IntType))
    assertEq(tcp("g", te2), FunctionType(List(), List(BoolType,IntType), IntType))
    assertFail(tcpss("def f(x: Int): Int = if(x) 3 else 2"))
    assertFail(tcpss("def f(b: Boolean): Boolean = if(b)  3 else 2"))
    val Ok(te3) = 
      tcpss("def fact(n: Int): Int = if(n <= 0) 1 else n * fact(n-1)")
    assertEq(tcp("fact", te3), FunctionType(List(), List(IntType), IntType))

    // Function applications
    assertInt(tcp("f(3)", te2)); assertInt(tcp("g(true, 4)", te2))
    assertInt(tcp("fact(4)", te3))
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
        "val four = 4",
        "def double(y: Int) : Int = 2*y",
        "val y = double(3)"
      ).mkString("\n")
    val Ok(te) = tcpss(script)
    assert(te("f") == FunctionType(List(), List(IntType), IntType))
    assert(te("g") == FunctionType(List(), List(BoolType,IntType), IntType))
    assert(te("fact") == FunctionType(List(), List(IntType), IntType))
    assert(te("h") == FunctionType(List(), List(IntType,BoolType), IntType))
    assert(te("four") == IntType)
    assert(te("double") == FunctionType(List(), List(IntType), IntType))
    assert(te("y") == IntType)
    val faultyScript = script+"; def ff(x: Int): Int = if(x) 3 else 2"
    assertFail(tcpss(faultyScript))
    assertFail(tcpss(script+"\n val y = double(2.2)"))
    // Functions aren't equality types
    val script2 = "def f(x: Int): Int = x+1; val x = f == f"
    assertFail(tcpss(script2))

    // Tests on block expressions
    assertInt(tcp("{"+script+"; fact(4) }"))
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
    }
    val script2 = "val x = #B2: Int; val ys = [x, 1]; val zs = [1, x]"
    tcpss(script2) match{ case Ok(te) => 
      assert(te("x") == IntType && te("ys") == ListType(IntType) &&
        te("zs") == ListType(IntType))
    }
    val script4 = "val x = #A4: Float; val y = #A1: Int"
    tcpss(script4) match{ case Ok(te) => 
      assert(te("x") == FloatType && te("y") == IntType)
      assertFail(tcpss("val ys = [y, x]", te))
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
    }
    val script13 = 
      "val x = #C3: Boolean; val y = #C4: Int; val w = if(x) y+3 else 4; val z = x == y"
    assertFail(tcpss(script13))
    val script14 = "val x = #A4: Float; val x2 = #A5: Float; val y = x == x; "+
      "val z = x2+x2; val w = x == x2"
    tcpss(script14) match{ case Ok(te) => 
      assert(te("x") == FloatType && te("z") == FloatType &&
        te("y") == BoolType && te("w") == BoolType)
    }
    tcpss("val y = #B3:Int; def f(): Int = y") match{ case Ok(te) => 
      assert(te("f") == FunctionType(List(), List(), IntType))
      assert(te("y") == IntType)
    }
  }

  // ==================================================================

  /** Tests writing to cells. */
  def cellWriteTests() = {
    tcpss("#A3 = 5") match{ case Ok(_) => {} }
    assertFail(tcpss("def f(y: Int): Int = 3; #A3 = f"))
    assertFail(tcpss("#A3 = true+5"))
    tcpss("val y = #B4: Float; #A3 = y") match{ case Ok(te) =>
      assert(te("y") == FloatType)
    }
    tcpss("val y = #B4:Float + #B5:Float; #A3 = y") match{ case Ok(te) =>
      assert(te("y") == FloatType)
    }
    // The following fails because head([]) doesn't evaluate to a cell type.
    assertFail(tcpss("#A1 = head([])"))
  }

 // ==================================================================

  /** Tests on lists. */
  def listTests() = {
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
    assertFail(tcpss("val x = #D0: Float; val y = #D3: Int; val eq = [x] == [y]"))
    tcpss("val xs = [#D0:Int]; val y = #D3:Int; val eq = xs == [y]") match{ 
      case Ok(te) =>
        assert(te("eq") == BoolType && te("xs") == ListType(IntType))
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
