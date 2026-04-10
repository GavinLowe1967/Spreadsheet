package spreadsheet

// import TypeT._
import TypeChecker0.TypeCheckRes
import TypeChecker._; 
import TypeEnv._
// import TypeChecker.TestHooks._
import Parser.parseAll
import StatementParser.TestHooks.{statement,statements}
// import NameExp.Name // Names of identifiers (Strings)

/** Helper functions for tests on the type checker. */
object TypeCheckerTest0{
  // The parser for expressions. 
  val expr = StatementParser.expParser.expr

  // Checking of expressions
  val typeCheckAndClose = TypeChecker.etc.typeCheckAndClose _

  var printErrors = false // Should error messages be printed?
  
  /* If printErrors, print error message. */
  def maybePrintError[A](x: Reply[A]) =
    if(printErrors) x match{ case FailureR(msg) => println(msg); case _ => {} }
  
  def assertFail[A](r: Reply[A]) = assert(r.isInstanceOf[FailureR], r)
  def assertOk[A](r: Reply[A]) = assert(r.isInstanceOf[Ok[A]], r)
  
  /* Assert that r is an Ok for type t. */
  def assertEq(r: TypeCheckRes, t: TypeT) = r match{
    case Ok((_,t1)) => assert(t == t1, s"Expected $t, found $t1")
  }

  /* Get new type environment. */
  def newEnv: TypeEnv = TypeEnv() // new TypeEnv(new NameMap, new Constraints)
  
  /* Parse and typecheck expression given by st. */
  def tcp(st: String, env: TypeEnv = newEnv) = {
    //println(env); sys.exit()
    val e = parseAll(expr, st); val res = typeCheckAndClose(env, e)
    maybePrintError(res); res
  }
    
  /* Parse and typecheck list of statements given by st. */
  def tcpss(st: String, env: TypeEnv = newEnv) = {
    val stmt = parseAll(statements, st); val res = typeCheckStmtList(env, stmt)
    maybePrintError(res); res
  }

  /** Check that r corresponds to an IntType. */
  def assertInt(r: TypeCheckRes) = r match{
    case Ok((te, IntType)) => {}
    case _ => sys.error(s"Expected Int, found $r")
  }

  def assertListInt(r: TypeCheckRes) = r match{
    case Ok((_, ListType(IntType))) => {}
    case _ => sys.error(s"Expected List[Int], found $r")
  }

  def assertListInt(te: TypeEnv, name: String) =  
    assert(te(name) == ListType(IntType))

  /** Check that name corresponds to a list of lists of Ints in te. */
  def assertListListInt(te: TypeEnv, name: String) =
    assert(te(name) == ListType(ListType(IntType)))

}

// ==================================================================


import TypeCheckerTest0._
import TypeT._

/** Tests on expressions. */
object TypeCheckerTestExpr{
  /** Basic tests. */
  private def basicTests() = {
    assertEq(tcp("#A3 : Boolean"), BoolType)
    assertInt(tcp("#A3: Int"))
    assertInt(tcp("2")); assertInt(tcp("2+3"))
    assertEq(tcp("4.5 + 2.4"), FloatType)
    assertEq(tcp("5%3"), IntType); assertFail(tcp("4.5 % 3.2"))
    assertFail(tcp("4.5 + 2"));  assertFail(tcp("4 + 2.4")) 
    assertFail(tcp("2+f+5"))
    assertFail(tcp("2+false+5")); assertFail(tcp("true+4"))
    assertEq(tcp("2 == 3"), BoolType); assertEq(tcp("2 <= 3"), BoolType)
    assertFail(tcp("true == 3")); assertFail(tcp("2 != false"))
    assertFail(tcp("true <= 3")); assertFail(tcp("2 <= false"))
    assertFail(tcp("2 && false")); assertFail(tcp("true || 4.0"))
    // println(tcp("true || false"))
    assertEq(tcp("true || false"), BoolType)
    // assertFail(tc(BinOp(StringExp("X"), "!=", StringExp("Y"))))
    //                                            TODO: need parser for Strings
    assertEq(tcp("toInt 3.4"), IntType); assertEq(tcp("toFloat 3"), FloatType)
    // Prefix operators
    assertEq(tcp("!true"), BoolType)
    assertEq(tcp("-{val x = 3; x+1}"), IntType)
    assertEq(tcp("- (3.4+2.5)"), FloatType)
    assertEq(tcp("()"), UnitType)

    // "if" expressions
    assertEq(tcp("if(2 == 3) 4.6 else 5.2"), FloatType)
    assertFail(tcp("if(2 == 3) 4 else 5.2"))
    assertFail(tcp("if(2 == 3) 4.6 else 5"))
    assertInt(tcp("if(2 == 3) 4 else 5"))
    assertFail(tcp("if(2) 4 else 5"))
    assertFail(tcp("if(2 == 3) 4 else false"))
    assertFail(tcp("if(2 == 3) 2+true else 4"))

    // Row and column arithmetic
    assertEq(tcp("#A+3"), ColumnType); assertEq(tcp("#F - 3"), ColumnType)
    assertEq(tcp("#5+3"), RowType); assertEq(tcp("#5 - 3"), RowType)
    assertEq(tcp("#D-#A"), IntType); assertEq(tcp("#6 - #4"), IntType)
    // "Expected Int or Column, found Float in 3.4 in ..."
    assertFail(tcp("#D-3.4")); assertFail(tcp("#99-true"))
  }

  /** Tests involving lists. */
  private def listTests() = {
    assertFail(tcp("[true, 4]"))
    assertListInt(tcp("[1,2,3]"))
    assertFail(tcp("[1, 3, false, 2, true]"))
    tcp("[]") match{ case Ok((te, ListType(TypeVar(t)))) => 
      assert(te(t) == AnyTypeConstraint) }
    tcp("[[]]") match{ case Ok((te, ListType(ListType(TypeVar(t))))) => 
      assert(te(t) == AnyTypeConstraint) }
    assertListInt(tcp("[#A1: Int, #A2: Int]"))
    assertFail(tcp("head(3)"))                            // IMPROVE error msg

    // Cons
    assertListInt(tcp("3 :: []"))
    assertFail(tcp("1:: [true]"))                         // IMPROVE error 
    assertEq(tcp("true :: []"), ListType(BoolType)) 
    tcp("[] :: []") match{ case Ok((te, ListType(ListType(TypeVar(t))))) => 
      assert(te(t) == AnyTypeConstraint) } 

    // Equality tests
    assertEq(tcp("[1] == [2]"), BoolType)
    assertEq(tcp("tail([1]) == []"), BoolType)
    assertFail(tcp("tail([1]) == tail([false])")) 
    assertFail(tcp("[1,2] == 3"))
    assertEq(tcp("[] == [3]"), BoolType)
    assertEq(tcp("[3] == []"), BoolType)
    // "Expected equality type, found (Int) => Int"
    assertFail(tcp("{def f(x:Int): Int = x; [f] == []}"))
    // "Expected List[equality type], found List[(Int) => Int]"
    assertFail(tcp("{def f(x:Int): Int = x; [] == [f]}"))
    // "Expected List[Int], found List[(Int) => Int]"
    assertFail(tcp(
      "{def f(x:Int): Int = x; val xs = []; xs == [3] && xs == [f]}"))
    // "Expected equality type, found (Int) => Int"
    assertFail(tcp(
      "{def f(x:Int): Int = x; val xs = []; xs == [3] && [f] == xs}"))
    assertFail(tcp(
      "{def f(x:Int): Int = x; val xs = []; [3] == xs && xs == [f]}"))
    assertFail(tcp(
      "{def f(x:Int): Int = x; val xs = []; [3] == xs && [f] == xs}"))
    assertEq(tcp("{def f[A <: Eq](x: A): Boolean = [] == [x]; f(3)}"), BoolType)

    // Mixing floats and ints
    assertFail(tcp("[1, 2.3]")) 
    assertFail(tcp("[1.6, 2]")) 

    tcp("head([])") match{ case Ok((te, TypeVar(t))) => 
      assert(te(t) == AnyTypeConstraint) } 

    // "to" and "until"
    assertEq(tcp("3 to 5"), ListType(IntType))
    assertEq(tcp("#3 until #5"), ListType(RowType))
    assertEq(tcp("#C until #Z"), ListType(ColumnType))
    assertFail(tcp("3.4 to 7.4"))
    // "Expected Int, found Row", etc.
    assertFail(tcp("1 until #3")); //  println(tcp("1 until #3"));
    assertFail(tcp("#3 to 7")); assertFail(tcp("#C until 5"))
    // "Expected Int, Row or Column, found Boolean", etc.
    assertFail(tcp("true until false")); assertFail(tcp("5.0 to 7.0"))
  }

  /**Tests on cell match expressions. */
  private def cellMatchTests() = {
    // printErrors = true
    val e1 = "#A1 match { case n: Int => n+1; case _: Float => 3; "+
      "case b: Boolean => if(b) 3 else 4; case Empty => 5 }"
    assertEq(tcp(e1), IntType)
    val e2 = 
      "Cell(#A, #1) match{ case n: Int => [3]; case Empty => []; case _ => [2] }"
    assertEq(tcp(e2), ListType(IntType))
    val e3 = "#A1 match{ case _: Int => []; case Empty => [3]; case _ => [2] }"
    assertEq(tcp(e3), ListType(IntType))
    tcp("#A1 match{ case Empty => [] }") match{ 
      case Ok((te, ListType(TypeVar(t)))) =>
        assert(te(t) == AnyTypeConstraint) }

    // "Expected Row, found Int"
    assertFail(tcp("Cell(#A,3) match{ case Empty => 4 }"))
    // "Expected Int or Float, found Boolean"
    assertFail(tcp("#A1 match{ case b: Boolean => b+1 }"))
    // "Empty list of branches for cell match expression"
    //assertFail(tcp("#A1 match { }"))
    // "Expected Int, found String"
    assertFail(tcp("#A1 match{ case n: Int => n+1; case Empty => \"empty\" }"))
    // printErrors = false
  }

  def untypedCellTests() = {
//printErrors = true
    println("untypedCellTests")
    assertEq(tcp("{ def f(x: Int): Int = x+1; f(#A1:Int) }"), IntType) 
    assertEq(tcp("{ def f(x: Int): Int = x+1; f(#A1) }"), IntType) 
    // "Couldn't find type(s) for cell expression(s)" 
    assertFail(tcp("{ def f[A](x: A): A = x; f(#A1) }")) 
    assertFail(tcp("{ def f[A](x: A): A = x; f(Cell(#B,#2)) }")) 
    assertFail(tcp("{ def f[A,B](x: A, y: B): A = x; f(#A1,#B2) }"))
    assertFail(tcp("{ def f[A](x: A, y: Int): A = x; f(#A1,#B2) }"))
    assertEq(tcp("{ def f[A](x: A): A = x; def g(x: Int): Int = x; g(f(#A3)) }"),
      IntType)
    assertEq(tcp("{ def f[A](x: A): A = x; def g(x: Int): Int = x; f(g(#A3)) }"),
      IntType)
    assertEq(tcp("if(#D1) 2 else 3"), IntType)
    assertEq(tcp("if(2+2==4) 3 else #E5"), IntType)
    assertFail(tcp("if(2+2==4) #E3 else 2"))
    assertFail(tcp("#A3 + 3"))
    assertEq(tcp("3 + #Q3"), IntType)
    assertFail(tcp("#V4 == 5"))
    assertFail(tcp("Cell(if(#A3 == 2) #B else #C, #2)"))
printErrors = false
  }

  /** Tests on explicitly typed expressions. */
  def typedTests() = {
    assertEq(tcp("3: Int"), IntType)
    assertEq(tcp("[]: List[Float]"), ListType(FloatType))
    assertFail(tcp("3: Float")) // "Expected Float, found Int"
    assertFail(tcp("(3+ 5.6): Float"))
    assertFail(tcp("2: A")) // "Type parameter A not in scope"
    assertFail(tcp("2 < 3 : Boolean"))
    assertEq(tcp("(2 < 3) : Boolean"), BoolType)
    assertEq(tcp("2 < 3 : Int"), BoolType)
  }

  def listComprehensionTests() = {
    assertEq(tcp("[x*x | x <- [1,2,3], x < 3]"), ListType(IntType))
    assertEq(tcp("[x*y | x <- [1,2,3], y <- [1,2,4], x < y]"), ListType(IntType))
    assertEq(tcp("[1 to x | x <- [4,5,6]]"), ListType(ListType(IntType)))
//printErrors = true
    assertFail(tcp("[x | x <- 5]"))
    assertFail(tcp("[x | x <- 3 to 7.0]"))
    assertFail(tcp("[x | x <- [2,4,5], x < 3.4]"))
    assertFail(tcp("[x && true | x <- [2,3,4]]"))
printErrors = false
  }

  /** Tests on tuple literals. */
  def tupleTests() = {
    assertEq(tcp("(1, 2.5 ,true ) "), 
      TupleType(List(IntType, FloatType, BoolType)))
    assertFail(tcp("(1,2,3,4,5,6,7,8,9,10,11,12,13)"))
    // println(tcp("(1,2,3,4,5,6,7,8,9,10,11,12)"))
    assertFail(tcp("(1, 2+true)"))
    assertFail(tcp("(1+true , 2)"))
    // Note: the extra parentheses are currently necessary
    // assertEq(tcp("get1From2((2+5, 3.5*1.2))"), IntType)
    assertEq(tcp("get1((2+5, 4.5))"), IntType)
    // "Expected (t51,t52,t53), found (Int,Int)"
    // assertFail(tcp("get1From3((2, 4))"))
    // "Application of overloaded function get3 with types ((A1,A2,A3)) => A3,
    // ((A1,A2,A3,A4)) => A3 can't be applied to argument of type (Int,Int)"
    assertFail(tcp("get3((2,4))"))
  }

  /** Tests on block expressions. */
  def blockTests() = {
    // Tests on block expressions
    assertFail(tcp("{ val x = three; x + 1 }"))
    assertFail(tcp("{ val x = 3; x + false }"))
    assertEq(tcp("{ #A3 = 2; 4 }"), IntType)
    // Unit-value blocks
    assertEq(tcp("{ #A3 = 2 }"), UnitType)
    assertEq(tcp("{ val x = 3 }"), UnitType)
    // Check x doesn't leak.
    assertFail(tcp("{ call{ val x = 3 }; x }"))
  }


  /** Tests on expressions. */
  def expTests() = {
    basicTests() // Basic expressions
    listTests()  // Lists
    cellMatchTests() // Cell match expressions
    untypedCellTests() // tests on untyped cell reads
    typedTests() // Tests on explicitly typed expressions.
    listComprehensionTests()
    tupleTests()
    // Repeated names 
    assertFail(tcpss("val x = 4; val x = 5"))
    assertFail(tcpss("val x = 4; def x(): Int = 5"))
    blockTests()
  }
}
