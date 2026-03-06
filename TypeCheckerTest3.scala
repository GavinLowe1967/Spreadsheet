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
      "def apply[A,B](f: A => B): A => B = { def ff(x: A): B = f(x); ff }\n"

    tcpss(addS+"val add3 = add(3)") match{ case Ok(te) =>
      assert(te("add") == FunctionType(
        List(), List(IntType), FunctionType(List(),List(IntType),IntType) ))
      assert(te("add3") == FunctionType(List(), List(IntType), IntType))
    }

    // Test on first const definition
    val constS1 = 
      constS+"val const3 = const(4); val constTrue = const(true)\n"+
        "val x = const3(3.4); val y = const3(false)\n"
    val constType = FunctionType(
      List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
      List(TypeParam("A")),
      FunctionType(List(), List(TypeParam("B")), TypeParam("A")) )
    tcpss(constS1) match{ case Ok(te) =>
      assert(te("const") == constType)
      assert(te("const3") == FunctionType( // forall B, B => Int
        List(("B",AnyTypeConstraint)), List(TypeParam("B")), IntType ) )
      assert(te("constTrue") == FunctionType( // forall B, B => Bool
        List(("B",AnyTypeConstraint)), List(TypeParam("B")), BoolType ) )
      assert(te("x") == IntType && te("y") == IntType)
    }

    // Test on second const definition. 
    val constS2 = 
      constSC+"val const3 = const(4); val constTrue = const(true)\n"+
        "val x = const3(3.4); val y = const3(false)\n"
    tcpss(constS2) match{
      case Ok(te) =>
        assert(te("const") == constType)
        assert(te("const3") == FunctionType( // forall B, B => Int
          List(("B",AnyTypeConstraint)), List(TypeParam("B")), IntType ) )
        assert(te("constTrue") == FunctionType( // forall B, B => Bool
          List(("B",AnyTypeConstraint)), List(TypeParam("B")), BoolType ) )
      assert(te("x") == IntType && te("y") == IntType)
    }

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

//println("======================================")

    val applyS1 = 
      applyS+constS+"val c = apply(const); val c3 = c(3); val x = c3(5);"+
        "val y = c3(true); val cT = c(true); val t = cT(5)"
    tcpss(applyS1) match{ case Ok(te) =>
      assert(te("apply") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("B"))),
        FunctionType(List(), List(TypeParam("A")), TypeParam("B")) ))
      //println(te("c"))
      te("c") match{
        case FunctionType(
          List((a, AnyTypeConstraint), (b, AnyTypeConstraint)),
          List(TypeParam(a1)),
          FunctionType(List(), List(TypeParam(b1)), TypeParam(a2)) 
        ) => assert(a1 == a2 && Set(a,b) == Set(a1,b1) && a != b)
          // Note: the ordering of the type parameters is
          // implementation-dependent.
      }
      // assert(te("c") == FunctionType(
      //   List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint)),
      //   List(TypeParam("A")),
      //   FunctionType(List(),List(TypeParam("B")),TypeParam("A")) ))
      //println(te("c3"))
      te("c3") match{
        case FunctionType(
          List((b,AnyTypeConstraint)), List(TypeParam(b1)), IntType
        ) => assert(b == b1) 
      }
      // assert(te("c3") == FunctionType(
      //   List(("B",AnyTypeConstraint)), List(TypeParam("B")), IntType) )
      assert(te("x") == IntType && te("y") == IntType)
      //println(te("cT"))
      te("cT") match{
        case FunctionType(
          List((b,AnyTypeConstraint)), List(TypeParam(b1)), BoolType
        ) => assert(b == b1) 
      }
      // assert(te("cT") == FunctionType(
      //   List(("B",AnyTypeConstraint)), List(TypeParam("B")),BoolType) )
      assert(te("t") == BoolType)
    }

// println("======================================")

    val s2 = 
      // foo(f)(x) = y
      "def foo[A,B,C](f: A => C): B => A => C = { "+
        "def ff(x: B): A => C = f; ff }\n "+
        "def g[A](y: A): Int = 3; val h = foo(g)"
    tcpss(s2) match{ case Ok(te) => 
      assert(te("foo") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint),
          ("C",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("A")), TypeParam("C"))),
        FunctionType(List(), List(TypeParam("B")),
          FunctionType(List(), List(TypeParam("A")), TypeParam("C")))
      ))
      assert(te("g") == FunctionType(
        List(("A",AnyTypeConstraint)), List(TypeParam("A")), IntType ))
      // println(te("h"))
      te("h") match{ // [A,B] B => A => Int
        case FunctionType(
          List((a, AnyTypeConstraint), ("B", AnyTypeConstraint)),
          List(TypeParam("B")),
          FunctionType(List(), List(TypeParam(a1)), IntType)
        ) => assert(a1 == a)
      }
    }

println("======================================")


    val s3 = 
      // foo(f)(x) = y
      "def foo[A,B,C](f: B => C): A => B => C = { "+
        "def ff(x: A): B => C = f; ff }\n "+
        "def g[A](y: A): Int = 3; val h = foo(g)"
    //println(tcpss(s3))
    tcpss(s3) match{ case Ok(te) => 
      assert(te("foo") == FunctionType(
        List(("A",AnyTypeConstraint), ("B",AnyTypeConstraint),
          ("C",AnyTypeConstraint)),
        List(FunctionType(List(), List(TypeParam("B")), TypeParam("C"))),
        FunctionType(List(), List(TypeParam("A")),
          FunctionType(List(), List(TypeParam("B")), TypeParam("C")))
      ))
      //println(te("h"))
      te("h") match{ // [A1,A]: A1 => A => Int for fresh name A1
        case FunctionType(
          List((a,AnyTypeConstraint), ("A",AnyTypeConstraint)),
          List(TypeParam("A")),
          FunctionType(List(), List(TypeParam(a1)), IntType)
        ) => assert(a1 == a)
      }
    }
    // Should have h: [A1,A]: A1 => A => Int for fresh name A1

  }

}
