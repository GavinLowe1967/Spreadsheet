package spreadsheet

// import TypeT._
import TypeChecker._; 
import TypeEnv._
import TypeChecker.TestHooks._
import Parser.parseAll; import ExpParser.expr
import StatementParser.{statement,statements}
// import NameExp.Name // Names of identifiers (Strings)

/** Helper functions for tests on the type checker. */
object TypeCheckerTest0{
  var printErrors = false // Should error messages be printed?
  
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

  def assertInt(r: Reply[TypeCheckRes]) = r match{
    case Ok((te, IntType)) => {}
    case _ => sys.error(s"Expected Int, found $r")
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

  def assertListInt(r: Reply[TypeCheckRes]) = r match{
    case Ok((_, ListType(IntType))) => {}
    case _ => sys.error(s"Expected List[Int], found $r")
  }

  def assertListInt(te: TypeEnv, name: String) =  
    assert(te(name) == ListType(IntType))

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
  /** Check that name corresponds to a list of lists of Ints in te. */
  def assertListListInt(te: TypeEnv, name: String) =
    assert(te(name) == ListType(ListType(IntType)))

}

// ==================================================================


import TypeCheckerTest0._
import TypeT._

/** Tests on expressions. */
object TypeCheckerTestExpr{
  /** Tests involving lists. */
  private def listTests() = {
    assertFail(tcp("[true, 4]"))
    assertEq(tcp("[1,2,3]"), ListType(IntType))
    assertFail(tcp("[1, 3, false, 2, true]"))
    tcp("[]") match{ case Ok((te, ListType(TypeVar(t)))) => 
      assert(te(t) == AnyTypeConstraint) }
    tcp("[[]]") match{ case Ok((te, ListType(ListType(TypeVar(t))))) => 
      assert(te(t) == AnyTypeConstraint) }
    //println(tcp("[#A1: Int, #A2: Int]"))
    tcp("[#A1: Int, #A2: Int]") match{ case Ok((te, ListType(IntType))) => {} }
    // ListType(TypeVar(tid)))) =>      assert(te(tid) == MemberOf(CellTypes)) }
    assertFail(tcp("head(3)"))                            // IMPROVE error msg

    // Cons
    assertEq(tcp("3 :: [ ]"), ListType(IntType)) 
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

    // Mixing floats and ints
    assertFail(tcp("[1, 2.3]")) // , ListType(FloatType))
    assertFail(tcp("[1.6, 2]")) // , ListType(FloatType)) 
  }


  /** Tests on basic expressions. */
  def expTests() = {
    assertEq(tcp("#A3 : Boolean"), BoolType)
    assertEq(tcp("#A3 : Int"), IntType)
    assertEq(tcp("2"), IntType); assertEq(tcp("2+3"), IntType)
    assertEq(tcp("4.5 + 2.4"), FloatType)
    assertFail(tcp("4.5 + 2")) // , FloatType)
    assertFail(tcp("4 + 2.4")) // , FloatType)
    assertFail(tcp("2+f+5"))
    assertFail(tcp("2+false+5")); assertFail(tcp("true+4"))
    assertEq(tcp("2 == 3"), BoolType); assertEq(tcp("2 <= 3"), BoolType)
    assertFail(tcp("true == 3")); assertFail(tcp("2 != false"))
    assertFail(tcp("true <= 3")); assertFail(tcp("2 <= false"))
    assertFail(tcp("2 && false")); assertEq(tcp("true || false"), BoolType)
    // assertFail(tc(BinOp(StringExp("X"), "!=", StringExp("Y"))))
    //                                            TODO: need parser for Strings

    // "if" expressions
    assertEq(tcp("if(2 == 3) 4.6 else 5.2"), FloatType)
    assertFail(tcp("if(2 == 3) 4 else 5.2")) // , FloatType)
    assertFail(tcp("if(2 == 3) 4.6 else 5")) // , FloatType)
    assertEq(tcp("if(2 == 3) 4 else 5"), IntType)
    assertFail(tcp("if(2) 4 else 5"))
    assertFail(tcp("if(2 == 3) 4 else false"))
    assertFail(tcp("if(2 == 3) 2+true else 4"))

    // Lists
    listTests()

    // Repeated names 
    assertFail(tcpss("val x = 4; val x = 5"))
    assertFail(tcpss("val x = 4; def x(): Int = 5"))

    // Tests on block expressions
    assertFail(tcp("{ val x = three; x + 1 }"))
    assertFail(tcp("{ val x = 3; x + false }"))
  }
}
