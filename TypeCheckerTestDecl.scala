package spreadsheet

import TypeT._
import TypeCheckerTest0._

/** Tests on simple declarations. */
object TypeCheckerTestDecl{

  /** Tests on single value declarations. */
  def valDecTests() = {
    // Value declarations
    val Ok(te) = tcpss("val four = 4")
    assertEq(tcp("four", te), IntType); assertEq(tcp("2+four", te), IntType)
    assertEq(tcp("5 == four", te), BoolType)
    assertFail(tcpss("val x = 2+false"))
    assertFail(tcpss("val y = 3 + 4.5"))
    tcpss("val y = 3 + 4") match{ case Ok(te) => assert(te("y") == IntType) }
    assertFail(tcpss("val x = head([])"))
    // The following succeeds, but gives an evaluation error.
    tcpss("val x = head([]: List[Int])") match{ case Ok(te) => 
      assert(te("x") == IntType) }
    // // The following succeeds, but gives an evaluation error.
    // tcpss("val x = head([])") match{ case Ok(te) => te("x") match{
    //   case TypeVar(t) => assert(te(t) == AnyTypeConstraint)
    // } }

    tcpss("val (x,(y,z)) = (2,(3.3,true))") match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == FloatType && te("z") == BoolType)
    }
    // "Cannot bind pattern to type Int at line 1 in (x,y) in val (x,y) = 3"
    assertFail(tcpss("val (x,y) = 3"))
    tcpss("val p = (2,3.0)") match{ case Ok(te) =>
      assert(te("p") == TupleType(IntType,FloatType)) }
    assertFail(tcpss("val ((x,y),z) = (2,3)"))
    // --- Repeated names
    // "x bound twice in declaration at line 1 in val (x,x) = (3,4)"
    assertFail(tcpss("val (x,x) = (3,4)"))
    assertFail(tcpss("val (x,(y,x)) = (2,(3,4))"))
    // "f has both val and def definitions at lines 1, 1."
    assertFail(tcpss("val (x,f) = (2,3); def f() = 5"))
    // "x has two val definitions at lines 1 and 1."
    assertFail(tcpss("val x = 3; val (y,x) = (4,5)"))
  }

  /** Tests on function declarations and applications. */
  private def funcDecTests() = { 
    val Ok(te) = tcpss("val four = 4")
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

  /** Tests on single declarations and function applications. */
  def singleDecTests() = { valDecTests(); funcDecTests() }

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
    // The reference to x, below, is a forward reference so not allowed.
    // "Forward reference to name x".
    assertFail(tcp("{ val x = 3; { val y = x+1; val x = 4; y } }"))
    assertFail(tcpss(
      "val x = 3; def f(y: Int): Int = { val z = x; val x = 4; y+z+x }"))
  }


  /** Tests on lists. */
  def listTests() = {
    tcpss("val xs = 3 :: []") match{ case Ok(te) => assertListInt(te,"xs") }
    tcpss("val xs = 1 :: 2 :: []") match{ case Ok(te) => assertListInt(te,"xs") }
    assertFail(tcpss("val xs = [1]; val ys = true::xs"))  // IMPROVE error
    assertFail(tcpss("val xs = []; val ys = 1::xs"))
    tcpss("val xs = []: List[Int]; val ys = 1::xs") match{ case Ok(te) => 
      assertListInt(te, "xs"); assertListInt(te, "ys") }
    // tcpss("val xs = []; val ys = 1::xs") match{ case Ok(te) => 
    //   assertListInt(te, "xs"); assertListInt(te, "ys") }
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
