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
    val stmt = parseAll(statements, st)
    val res = typeCheckStmtList(env, stmt, true)
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

