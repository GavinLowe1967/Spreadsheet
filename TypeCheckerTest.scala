package spreadsheet

import TypeT._
import TypeCheckerTest0._

/** Tests on the type checker. */
object TypeCheckerTest{

  /** Tests on single declarations and function applications. */
  def singleDecTests() = {
    // Value declarations
    val Ok(te) = tcpss("val four = 4")
    assertNum(tcp("four", te)); assertNum(tcp("2+four", te))
    assertEq(tcp("5 == four", te), BoolType)
    assertFail(tcpss("val x = 2+false"))
    tcpss("val y = 3 + 4.5") match{ case Ok(te) => assert(te("y") == FloatType) }
    tcpss("val y = 3 + 4") match{ case Ok(te) => assertNum(te, "y") }

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
    assertNum(te, "four")
    val faultyScript = script+"; def ff(x: Int): Int = if(x) 3 else 2"
    assertFail(tcpss(faultyScript))
    // Functions aren't equality types
    val script2 = "def f(x: Int): Int = x+1; val x = f == f"
    assertFail(tcpss(script2))

    // Tests on block expressions
    assertEq(tcp("{"+script+"; fact(4) }"), IntType)
    assertFail(tcp("{"+script+"; fact(true) }"))
    assertFail(tcp("{"+faultyScript+"; fact(4) }"))
  }

  // ==================================================================

  /** Tests on cell expressions. */
  def cellTests() = {
    val script = "val x = Cell(#B, #2); def f(y: Int): Int = 3"
    tcpss(script) match{ case Ok(te) => 
      // x is just a Cell here
      te("x") match{ case TypeVar(t) => assert(te(t) == MemberOf(CellTypes)) }
      tcp("if(x) 3 else 4", te) match{ case Ok((te2, TypeVar(tv))) =>
        assert(te2(tv) == NumTypeConstraint)
        assert(te2("x") == BoolType) // But now it's a Bool
        assertFail(tcp("f(x)", te2))
      }
    }
    val script2 = "val x = Cell(#B, #2); val ys = [x, 1]"
    tcpss(script2) match{ case Ok(te) => 
      te("x") match{ case TypeVar(tv) =>
        assert(te(tv) == NumTypeConstraint)
        assert(te("ys") == ListType(TypeVar(tv)))
      }
    }
    val script3 = "val x = Cell(#B, #2); val ys = [1, x]"
    tcpss(script3) match{ case Ok(te) => 
      te("x") match{ case TypeVar(tv) => 
        assert(te(tv) == MemberOf(TypeT.NumTypes)) 
        assert(te("ys") == ListType(TypeVar(tv)))
      }
    }
    val script4 = "val x = Cell(#B, #2); val y = #A1; val ys = [y, x]"
    tcpss(script4) match{ case Ok(te) => 
      te("x") match{ case TypeVar(t) =>
        assert(te("y") == TypeVar(t) && te("ys") == ListType(TypeVar(t)))
        assert(te(t) == MemberOf(CellTypes))
        tcp("[y, 2]", te) match{ case Ok((te2, ListType(TypeVar(tv)))) =>
          assert(te2(tv) == NumTypeConstraint)
          assertNum(te2, "x") // Now it's a NumType
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

    assertNum(tcp("if(2+2 == 4) 3 else #A2"))
    assertEq(tcp("if(2+2 == 4) #B3 else false"), BoolType)
    tcp("if(2+2 == 4) #B3 else #A5") match{ case Ok((_, TypeVar(_))) => {} }

    val script8 = "val x = #C3; val y = x; def f(x: Int): Int = if(y) x else 3"
    tcpss(script8) match{ case Ok(te) => 
      assert(te("x") == BoolType && te("y") == BoolType && 
        te("f") == FunctionType(List(), List(IntType), IntType))
    }
    val script9 = 
      "val x = #C3; val y = x; def f(x: Int): Int = if(x==y) x else 3"
    tcpss(script9) match{ case Ok(te) => 
      assert(te("x") == IntType && te("y") == IntType && 
        te("f") == FunctionType(List(), List(IntType), IntType))
    }
    val script10 = 
      "val x = #C3; val y = x; val z = { val x = 3; y && false }"
    tcpss(script10) match{ case Ok(te) => 
      assert(te("x") == BoolType && te("y") == BoolType && te("z") == BoolType)
    }
    val script11 = "val x = #C3; val y = x; val z = { val x = 3; y == 5 }"
    tcpss(script11) match{ case Ok(te) => 
      assertNum(te, "x"); assertNum(te, "y"); assert(te("z") == BoolType)
    }
    val script12 = "val x = #C3; def f(y: Int): Int = x"
    tcpss(script12) match{ case Ok(te) => assert(te("x") == IntType) }

    tcpss("val x = #C3; val y = x == 3") match{ case Ok(te) =>
      assertNum(te, "x"); assert(te("y") == BoolType)
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
    val script14 = "val x = #A4; val x2 = #A5; val y = x == x; "+
      "val z = x2+x2; val w = x == x2"
    tcpss(script14) match{ case Ok(te) => 
      te("x") match{ case TypeVar(tId) => 
        assert(te(tId) == NumTypeConstraint); 
        assert(te("x2") == TypeVar(tId)); assert(te("z") == TypeVar(tId))
      }
      assert(te("w") == BoolType); assert(te("y") == BoolType)
    }

  }

  // ==================================================================

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

 // ==================================================================

  /** Tests on lists. */
  def listTests() = {
    tcpss("val xs = 3 :: []") match{ case Ok(te) => assertListNum(te, "xs") }
    tcpss("val xs = 1 :: 2 :: []") match{ case Ok(te) => assertListNum(te,"xs") }
    assertFail(tcpss("val xs = [1]; val ys = true::xs"))  // IMPROVE error
    tcpss("val xs = []; val ys = 1::xs") match{ case Ok(te) => 
      assertListNum(te, "xs"); assertListNum(te, "ys") }
    tcpss("val x = #A1; val xs = [x, #A2]; val y = x+3") match{ case Ok(te) => 
      assertListNum(te, "xs") }
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
    tcpss("val xs = [1,2,3]; val x = head(xs)") match{
      case Ok(te) => assertListNum(te, "xs"); assertNum(te, "x")
    }
    tcpss("val xs = [true, false]; val x = head(xs)") match{ case Ok(te) => 
      assert(te("xs") == ListType(BoolType) && te("x") == BoolType)
    }
    tcpss("val xs = [[1,2],[3]]; val x = head(xs)") match{ case Ok(te) => 
      assertListListNum(te, "xs"); assertListNum(te, "x")
    }
    tcpss("val xs = [[1,2],[3]]; val ys = tail(xs)") match{ case Ok(te) => 
      assertListListNum(te, "xs"); assertListListNum(te, "ys")
    }
  }

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
      assertNum(te, "y") 
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
      assert(te("f") == idFunctionType); assertNum(te, "w") //te("w") == IntType)
    }
    assertFail(tcpss("def f[A](x: A): Boolean = x == x"))
  }

  // ==================================================================

  /** Tests using type constraints. */
  def typeConstraintTests() = {
    val script8 = 
      "def f[A <: Eq](x: A): Boolean = x == x; val y = f(3); val z = f(#B4)"
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
    tcpss("def f[A <: Num](x: A): A = 3+x") match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A", NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
    }
    tcpss("def f[A <: Num](x: A): A = x+3") match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A", NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
    }
    assertFail(tcpss("def f[A, B](x: A): B = x"))
    tcpss("val y = #B3; def f(): Int = y") match{ case Ok(te) => 
      assert(te("f") == FunctionType(List(), List(), IntType))
      assert(te("y") == IntType)
    }
    tcpss("val y = #B3; def f[A <: Num](): A = y") match{ case Ok(te) =>
      assert(te("y") == IntType)
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    tcpss("val y = #B3; def f[A <: Num](): A = y+3") match{ case Ok(te) =>
      assert(te("y") == IntType)
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    tcpss("val y = #B3; def f[A <: Num](): A = 3+y") match{ case Ok(te) =>
      assert(te("y") == IntType)
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    tcpss("def f[A <: Num](): A = 3") match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A",NumTypeConstraint)), List(), TypeParam("A")) )
    }
    // Following two fail, because #B3 would need to hold value of type A, for
    // all A.
    assertFail(tcpss("def f[A](x: A): A = if(true) x else #B3"))
    assertFail(tcpss("def f[A](x: A): A = #B3"))
    // Below, x doesn't satisfy Num
    val script12 = "def f[A <: Eq](x: A): Boolean = g(x); "+
      "def g[B <: Num](y: B): Boolean = y == y"
    assertFail(tcpss(script12))

  }


  // =======================================================

  def main(args: Array[String]) = {
    // printErrors = true
    TypeCheckerTestExpr.expTests()
    singleDecTests()
    scriptTests()
    cellTests()
    cellWriteTests()
    listTests()
    polyTests()
    typeConstraintTests()
    // printErrors = true
 
    println("Done")
  }

}
