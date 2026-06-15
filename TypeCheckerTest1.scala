package spreadsheet

import TypeT._
import TypeCheckerTest0._

/** Tests on the type checker.  Tests on scripts, but expluding polymorphism
  * and higher-order functions. */
object TypeCheckerTest1{
  def assertionTests() = {
    assertOk(tcpss("assert(2+2 == 5)")); assertOk(tcpss("assert(true, \"X\")"))
    // "Expected Boolean, found Int" (for both))
    assertFail(tcpss("assert(42)")); assertFail(tcpss("assert(42, \"X\")"))
    // "Expected String, found Int"
    assertFail(tcpss("assert(true, 5)"))
  }

  // ==================================================================

  /** Tests on cell expressions. */
  def cellTests() = {
    val script = "val x = Cell(#B, #2): Int"
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

    // Untyped cell expressions
//printErrors = true
    tcpss("def f(x: Int): Int = x+1; val x = f(#A1)") match{ case Ok(te) =>
      assert(te("x") == IntType) }
    assertFail(tcpss("def f[A](x: A): A = x; val x = f(#A1)"))
    tcpss("val x = if(#C3) 2.5 else 3.5") match{ case Ok(te) => 
      assert(te("x") == FloatType) }
//printErrors = false
  }

  // ==================================================================

  /** Tests writing to cells. */
  def cellWriteTests() = {
    assertOk(tcpss("#A3 = 5")) // match{ case Ok(_) => {} }
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

//printErrors = true
    assertOk(tcpss("def f(x: Int): Int = x+1; #A2 = f(#A1)"))
    assertFail(tcpss("def id[A](x: A): A = x; #A2 = id(#A1)"))
    assertFail(tcpss("def id[A](x: A): A = x; val y = id(#A1)"))
    assertOk(tcpss("Cell(if(#A1) #B else #C, #1) = 3"))
    assertOk(tcpss("def f(x: Int): Int = #A5"))
    assertOk(tcpss("def id[A](x: A): A = x; def f(x: Int): Int = id(#A1)"))
    // Attempts to find error from closing in FunctionDeclaration case.  All
    // of these give errors earlier in the typechecking.
    assertFail(tcpss(
      "def id[A](x: A): A = x; def f(x: Int): Int = { val x = id(#A1); 3}"))
    assertFail(tcpss("def f(x: Int): Int = if(#A3 == 2) 1 else 2"))
    assertFail(tcpss("def f[A](x: Int): A = #A3"))
    assertFail(tcpss("def f(x: Int): Boolean = (#A2 == #A3)")) 
    assertFail(tcpss(
      "def id[A](x: A): A = x; def f(x: Int): Boolean = id(#A3) == #A4"))
    // Following test shows why closing in type checking of a
    // FunctionDeclaration is necessary: it captures that the type of #A1 is
    // unknown.
    assertFail(tcpss(
      "def f[A,B](x: A, y: B): A = x; def g(x: Int): Int = f(x, #A1)"))

    // "for" statements
    assertFail(tcpss("for(x <- [#A2]) #B2 = 0"))
    assertOk(tcpss("def id[A](x: A): A = x; for(if id(#A3)) #B3 = 1"))
    assertFail(tcpss("for(if #A1 == #A2) #B4 = 1"))
    assertOk(tcpss("for(if #A1) val x = 3"))
    // The following two tests show why the closing is necessary in the case
    // of a Filter to get the error in the right place.
    assertFail(tcpss(
      "def f[A,B](x: A, y: B): A = x; for(if f(true, #A1)) #A3 = 2"))
    assertFail(tcpss(
      "def f[A,B](x: A, y: B): A = x; for(if f(true, #A1)) val x = 2+3.4"))
    assertFail(tcpss("def f[A,B](x: A, y: B): A = x; for(if f(true, #A1)){ }"))
// printErrors = false
  }

 // ==================================================================


  /** Tests on expressions using actual type parameters. */
  def typeParamTests() = {
//printErrors = true
    // "Type parameters applied to non-function x ..."
    assertFail(tcpss("val x = 3; val y = x[Int]"))
    // "Wrong number of type parameters for function f ..."
    assertFail(tcpss("def f[A](x: A) = x; val y = f[Int,Float](3)"))
    tcpss("def f[A](x: A) = x; val y = f[Int](3)") match{ case Ok(te) =>
      assert(te("y") == IntType) }
    tcpss("def f[A](x: A)(y: A) = x; val f1 = f[Int](3)") match{ case Ok(te) =>
      assert(te("f1") == FunctionType(List(),List(IntType),IntType)) }
    tcpss("def f[A](x: Int)(y: A) = y; val f1 = f[Float](3)") match{ 
      case Ok(te) =>
        assert(te("f1") == FunctionType(List(),List(FloatType),FloatType)) }
    tcpss("def f[A](x: A) = [(x,3)]; val f1 = f[Float]") match{ case Ok(te) =>
      assert(te("f1") == FunctionType(
        List(), List(FloatType), ListType(TupleType(List(FloatType,IntType))) ))}
    // "Actual type parameter (Int) => Int does not satisfy type constraint Eq"
    assertFail(tcpss("def f[A <: Eq](x: A) = x; val f1 = f[Int => Int]"))
    val script = "def f[A](x:Int) = 3; def f[A,B](x:Float) = 4; "+
      "val f1 = f[Int]; val f2 = f[String,Boolean]"
    tcpss(script) match{ case Ok(te) => 
      assert(te("f1") == FunctionType(List(),List(IntType),IntType))
      assert(te("f2") == FunctionType(List(),List(FloatType),IntType))
    }
    // === Overloading
    // "Cannot resolve overloaded name f at line 1 in f[Int]"
    assertFail(tcpss("def f[A](x:Int) = 3; def f[A](x:A) = 4; val f1 = f[Int]"))
    // "Cannot resolve overloaded name f with types ..."
    assertFail(tcpss("def f[A <: Eq](x:Int) = 3; def f[A,B](x:A) = 4; "+
      "val f1 = f[Int => Int]"))
   
    tcpss("def f[A <: Eq](x:Int) = 3; def f[A](x:A) = 4.0; "+
      "val f1 = f[Int => Int]") match{ case Ok(te) => 
        assert(te("f1") == FunctionType(
          List(), List(FunctionType(List(),List(IntType),IntType)), FloatType)
        ) }

    // ===== TypedExps
//printErrors = true
    tcpss("def f[A](x: A) = 3; val f1 = f[Float]: Float => Int") match{ 
      case Ok(te) =>
        assert(te("f1") == FunctionType(List(),List(FloatType),IntType)) }
    tcpss("def f[A](x: Int)(y: A) = x; val f1 = f[Float](3): Float => Int"
    ) match{ case Ok(te) => 
        assert(te("f1") == FunctionType(List(),List(FloatType),IntType)) }
    // "Expected Int, found Float" ** IMPROVE error message
    assertFail(tcpss("def f[A](x: A) = 3; val f1 = f[Float]: Int => Int"))
    // === Overloading
    // Ambiguous use of overloaded name f at line 1"
    assertFail(tcpss(
      "def f[A](x:Int) = 3; def f[A](x:A) = 4; val f1 = f[Int]: Int => Int") )
    // match{ case Ok(te) =>
      //   assert(te("f1") == FunctionType(List(),List(IntType),IntType)) }
    // Here the type resolves the choice
    tcpss("def f[A](x:Int) = 3; def f[A](x:A) = 4.0; "+
      "val f1 = f[Int]: Int => Float") match{ case Ok(te) =>
        assert(te("f1") == FunctionType(List(),List(IntType),FloatType)) }

    // At present, the first instance is chosen.
// FIXME FROM HERE
    // Ambiguous use of overloaded name f at line 1
    assertFail(tcpss(
      "def f[A](x:Int) = 3; def f[A](x:A) = 4; val f2 = f[Int]: Int => Int")) 
    // match{ case Ok(te) =>
    // assert(te("f1") == FunctionType(List(),List(IntType),IntType)) }
    // Overloaded name f with types (Int) => Int, (Int) => Int is not of type
    // (Int) => Float
    assertFail(tcpss(
      "def f[A](x:Int) = 3; def f[A](x:A) = 4; val f3 = f[Int]: Int => Float"))

    // ======== Function applications
    tcpss("def f[A](x: A) = 3; val y = f[Float](2.3)") match{ case Ok(te) => 
      assert(te("y") == IntType) }
    // Overloading; amgiguous
    assertFail(tcpss(
      "def f[A](x:Int) = 3; def f[A](x:A) = 4.0; val y = f[Int](3)"))
    //  match{ case Ok(te) => assert(te("y") == IntType) }
    val script2 = "def f[A](x:Int) = 3; def f[A,B](x:Float) = 4.0; "+
      "val y1 = f[Int](2); val y2 = f[String,Boolean](2.0)"
    tcpss(script2) match{ case Ok(te) => 
      assert(te("y1") == IntType && te("y2") == FloatType) }
     // "Actual type parameter (Int) => Int does not satisfy type constraint Eq"
    assertFail(tcpss("def f[A <: Eq](x: Int) = x; val y1 = f[Int => Int](3)"))
    tcpss("def f[A <: Eq](x: Int) = x; def f[A](x: Float) = 3.0; "+
      "val y1 = f[Int => Int](3.0)") match{ case Ok(te) =>
        assert(te("y1") == FloatType) }
    // "Expected Float, found Int"
    assertFail(tcpss("def f[A <: Eq](x: Int) = x; def f[A](x: Float) = 3.0; "+
      "val y1 = f[Int => Int](3)"))
    // "Ambiguous application of overloaded name f"
    assertFail(tcpss(
      "def f[A](x:Int) = 3; def f[A](x:A) = 4.0; val y1 = f[Int](4)"))
      //match{  case Ok(te) => assert(te("y1") == IntType) }
    assertFail(tcpss(
      "def f[A](x:A) = 4.0; def f[A](x:Int) = 3; val y1 = f[Int](5)"))
    // match{      case Ok(te) => assert(te("y1") == FloatType) }
    assertFail(tcpss(
      "def f[A](x:Int) = 3; def f[A](x:A) = 4.0; val y = f[Int](6)"))
    //match{    case Ok(te) => assert(te("y") == IntType) }
// printErrors = false
  }
}
