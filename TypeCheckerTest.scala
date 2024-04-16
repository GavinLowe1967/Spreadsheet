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

  /** Check that r corresponds to a numeric type. */
  def assertNum(r: Reply[TypeCheckRes]) = r match{
    case Ok((te, TypeVar(tv))) => assert(te(tv) == NumTypeConstraint)
    case _ => sys.error(s"Expected numeric result, found $r")
  }

  /** Check that name corresponds to a numeric type in te. */
  def assertNum(te: TypeEnv, name: String) = te(name) match{
    case TypeVar(tv) => assert(te(tv) == NumTypeConstraint)
    case t => sys.error(s"Expected numeric result, found $t")
  }

  /** Check that r corresponds to a numeric list type. */
  def assertListNum(r: Reply[TypeCheckRes]) = r match{
    case Ok((te, ListType(TypeVar(tv)))) =>  assert(te(tv) == NumTypeConstraint)
    case _ => sys.error(s"Expected numeric list result, found $r")
  }

  /** Check that name corresponds to a numeric list type in te. */
  def assertListNum(te: TypeEnv, name: String) = te(name) match{
    case ListType(TypeVar(tv)) => assert(te(tv) == NumTypeConstraint)
    case t => sys.error(s"Expected numeric list result, found $t")
  }

  /** Check that name corresponds to a list of lists of numeric values in te. */
  def assertListListNum(te: TypeEnv, name: String) = te(name) match{
    case ListType(ListType(TypeVar(tv))) => assert(te(tv) == NumTypeConstraint)
  }

  // ==================================================================

  /** Tests on basic expressions. */
  def expTests() = {
    assertNum(tcp("2"))  // assertEq(tcp("2"), IntType); 
    assertNum(tcp("2+3")) //  assertEq(tcp("2+3"), IntType)
    assertFail(tcp("2+f+5"))
    assertFail(tcp("2+false+5")); assertFail(tcp("true+4"))
    assertEq(tcp("2 == 3"), BoolType); assertEq(tcp("2 <= 3"), BoolType)
    assertFail(tcp("true == 3")); assertFail(tcp("2 != false"))
    assertFail(tcp("true <= 3")); assertFail(tcp("2 <= false"))
    assertFail(tcp("2 && false")); assertEq(tcp("true || false"), BoolType)
    // assertFail(tc(BinOp(StringExp("X"), "!=", StringExp("Y"))))
    //                                            TODO: need parser for Strings

    // "if" expressions
    assertNum(tcp("if(2 == 3) 4 else 5"))
    // assertEq(tcp("if(2 == 3) 4 else 5"), IntType)
    assertFail(tcp("if(2) 4 else 5"))
    assertFail(tcp("if(2 == 3) 4 else false"))
    assertFail(tcp("if(2 == 3) 2+true else 4"))

    // Lists
    assertFail(tcp("[1,2] == 3"))
    assertFail(tcp("[true, 4]"))
  }

  // ==================================================================

  /** Tests on single declarations and function applications. */
  def singleDecTests() = {
    // Value declarations
    val Ok(te) = tcpss("val four = 4")
    assertNum(tcp("four", te)) // assertEq(tcp("four", te), IntType); 
    assertNum(tcp("2+four", te)) //  assertEq(tcp("2+four", te), IntType)
    assertEq(tcp("5 == four", te), BoolType)
    assertFail(tcpss("val x = 2+false"))

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
    assertNum(te, "four") // assert(te("four") == IntType)
    val faultyScript = script+"; def ff(x: Int): Int = if(x) 3 else 2"
    assertFail(tcpss(faultyScript))
    // Functions aren't equality types
    val script2 = "def f(x: Int): Int = x+1; val x = f == f"
    assertFail(tcpss(script2))
    // Repeated names 
    assertFail(tcpss("val x = 4; val x = 5"))
    assertFail(tcpss("val x = 4; def x(): Int = 5"))

    // Tests on block expressions
    // printErrors = true
    assertFail(tcp("{ val x = three; x + 1 }"))
    assertFail(tcp("{ val x = 3; x + false }"))
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
      tcp("if(x) 3 else 4", te) match{ case Ok((te2, TypeVar(t1))) =>
        assert(te2(t1) == NumTypeConstraint)
        assert(te2("x") == BoolType) // But now it's a Bool
        assertFail(tcp("f(x)", te2))
      }
    }
    val script2 = "val x = Cell(#B, #2); val ys = [x, 1]"
    tcpss(script2) match{ case Ok(te) => 
      assertNum(te, "x") // assert(te("x") == IntType)
      assertListNum(te, "ys") // assert(te("ys") == ListType(IntType))
    }
    val script3 = "val x = Cell(#B, #2); val ys = [1, x]"
    tcpss(script3) match{ case Ok(te) => 
      assertNum(te, "x"); assertListNum(te, "ys")
      // assert(te("x") == IntType && te("ys") == ListType(IntType))
    }
    val script4 = "val x = Cell(#B, #2); val y = #A1; val ys = [y, x]"
    tcpss(script4) match{ case Ok(te) => 
      te("x") match{ case TypeVar(t) =>
        // assertNum(te, "y"); assertListNum(te, "ys")
        assert(te("y") == TypeVar(t) && te("ys") == ListType(TypeVar(t)))
        assert(te(t) == MemberOf(CellTypes))
        tcp("[y, 2]", te) match{ case Ok((te2, ListType(TypeVar(tv)))) =>
          assert(te2(tv) == NumTypeConstraint)
        // tcp("[y, 2]", te) match{ case Ok((te2, ListType(IntType))) => 
          assertNum(te2, "x") // Now it's a NumType
          // assert(te2("x") == IntType) // Now it's an Int
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
    // assertEq(tcp("if(2+2 == 4) 3 else #A2"), IntType)
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
      //assert(te("x") == IntType && te("y") == IntType && te("z") == BoolType)
    }
    val script12 = "val x = #C3; def f(y: Int): Int = x"
    tcpss(script12) match{ case Ok(te) => assert(te("x") == IntType) }

    tcpss("val x = #C3; val y = x == 3") match{ case Ok(te) =>
      assertNum(te, "x"); assert(te("y") == BoolType)
      // assert(te("x") == IntType && te("y") == BoolType)
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
    assertListNum(tcp("[1,2,3]"))
    //assertEq(tcp("[1,2,3]"), ListType(IntType))
    assertFail(tcp("[1, 3, false, 2, true]"))
    tcp("[]") match{ case Ok((te, ListType(TypeVar(t)))) => 
      assert(te(t) == AnyTypeConstraint) }
    assertListNum(tcp("3 :: [ ]")) //assertEq(tcp("3 :: [ ]"), ListType(IntType))
    tcpss("val xs = 3 :: []") match{ case Ok(te) => 
      assertListNum(te, "xs") } //assert(te("xs") == ListType(IntType)) }
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
      assertListNum(te, "xs") } //      assert(te("xs") == ListType(IntType)) }
    assertFail(tcp("1:: [true]"))
    assertFail(tcpss("val xs = [1]; val ys = true::xs"))
    tcpss("val xs = []; val ys = 1::xs") match{ case Ok(te) => 
      assertListNum(te, "xs"); assertListNum(te, "ys") }
    // assert(te("xs") == ListType(IntType) && te("ys") == ListType(IntType)) }
    assertEq(tcp("[1] == [2]"), BoolType)
    tcpss("val xs = [#A1, #A2]") match{ case Ok(te) => 
      te("xs") match{ case ListType(TypeVar(tid)) => 
        assert(te(tid) == MemberOf(CellTypes)) }}
    tcpss("val x = #A1; val xs = [x, #A2]; val y = x+3") match{ case Ok(te) =>
      assertListNum(te, "xs") } //      assert(te("xs") == ListType(IntType)) }
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
    assertEq(tcp("tail([1]) == []"), BoolType)
    assertFail(tcp("tail([1]) == tail([false])")) 
    tcpss("val xs = [1,2,3]; val x = head(xs)") match{ case Ok(te) => 
      assertListNum(te, "xs"); assertNum(te, "x")
      // assert(te("xs") == ListType(IntType) && te("x") == IntType)
    }
    tcpss("val xs = [true, false]; val x = head(xs)") match{ case Ok(te) => 
      assert(te("xs") == ListType(BoolType) && te("x") == BoolType)
    }
    tcpss("val xs = [[1,2],[3]]; val x = head(xs)") match{ case Ok(te) => 
      assertListListNum(te, "xs"); assertListNum(te, "x")
      // assert(te("xs") == ListType(ListType(IntType)) &&
      //   te("x") == ListType(IntType))
    }
    tcpss("val xs = [[1,2],[3]]; val x = tail(xs)") match{
      case Ok(te) => 
        assertListListNum(te, "xs"); assertListListNum(te, "x")
        // assert(te("xs") == ListType(ListType(IntType)) && 
        //   te("x") == ListType(ListType(IntType)))
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
      assertNum(te, "y") //      assert(te("y") == IntType)
    }
    val script = "def apply[A, B](f: A => B, x: A) : B = f(x);"+
      "def g(x: Int): Boolean = x > 2"
    tcpss(script+"; val b = apply(g, 4)") match{ case Ok(te) => 
      assert(te("b") == BoolType) }
    assertFail(tcpss("def f[A](x: A, y: A): A = x; val y = f(3, true)"))
    assertFail(tcpss(script+"; val c = apply(g, true)"))
    tcpss("def f[A](x: A): A = x; val y = f(3)") match{ case Ok(te) => 
      assert(te("f") == FunctionType(
        List(("A",AnyTypeConstraint)), List(TypeParam("A")), TypeParam("A")))
      assertNum(te, "y")
    }
    tcpss("def f[A](x: A, y: A): A = x; val y = f(3, 4)") match{ case Ok(te) =>
      assert(te("f") == FunctionType(
        List(("A",AnyTypeConstraint)), 
        List(TypeParam("A"), TypeParam("A")), TypeParam("A")))
      assertNum(te, "y")
    }

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
    assertFail(tcpss("def f(x: Int): A = #B3")) // missing tparam

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
      assert(te("f") == idFunctionType) // && te("w") == IntType)
      assertNum(te, "w")
    }

    assertFail(tcpss("val xs = head(3)"))   // IMPROVE error msg
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
    tcpss("def f[A <: Num](x: A): A = x+x; val y = f(3); val z = f(1.2)") match{
      case Ok(te) =>
        assert(te("f") == FunctionType(
          List(("A",NumTypeConstraint)), List(TypeParam("A")), TypeParam("A")) )
        assertNum(te, "y") // assert(te("y") == IntType)
        assert(te("z") == FloatType)
    }
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
    //    assertFail(tcpss("def f[A <: Num](x: A): A = 3+x"))
    printErrors = true
    println("XXX")
    println(tcpss("def f[A <: Num](): A = 3"))
// XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
// Should work
    assertFail(tcpss("def f[A <: Num](x: A): A = x+3"))
    println("XXX")
    // printErrors = false
    assertFail(tcpss("def f[A, B](x: A): B = x"))
    tcpss("val y = #B3; def f(): Int = y") match{ case Ok(te) => 
      assert(te("f") == FunctionType(List(), List(), IntType))
      assert(te("y") == IntType)
    }
    assertFail(tcpss("val y = #B3; def f[A <: Num](): A = y"))
    assertFail(tcpss("def f[A](x: A): A = if(true) x else #B3"))
    assertFail(tcpss("def f[A](x: A): A = #B3"))

  }


  // =======================================================

  def main(args: Array[String]) = {

    // printErrors = true
    expTests()
    singleDecTests()
    scriptTests()
    cellTests()
    cellWriteTests()
    listTests()
    polyTests()
    typeConstraintTests()

    printErrors = true
    tcpss("val x = 3 + 4.6") match{ case Ok(te) => assert(te("x") == FloatType) }

    tcpss("val x = 4.6 + 3") match{ case Ok(te) => assert(te("x") == FloatType) }

    // TODO
    // println(tcpss("val xs = [3, 5.5]"))

    println("Done")
  }

}
