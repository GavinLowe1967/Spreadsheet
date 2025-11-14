package spreadsheet

import Parser._

/** Common functions between the different parser test objects. */
trait ParserTest0{
  // import ExpParser.expr
  val expr = DeclarationParser.expParser.expr

  def isWhite(c: Char) = c == ' ' || c == '\t' || c == '\n'
   
  // Remove leading and training whilespace from st
  def trim(st: String) =
    st.dropWhile(isWhite).reverse.dropWhile(isWhite).reverse
    
  // Check that ext matches st
  def checkExtent(ext: Extent, st: String) = {
    assert(ext != null, s"Null extent with $st")
    val st1 = trim(st); val st2 = ext.asString
    assert(st2 == st1, s"\"$st2\" != \"$st1\"")
  }

  // Parse as expression
  def p0(st: String): Exp = parseAll(expr, st)
  
  // Parse as expression, and check extent
  def p(st: String): Exp = {
    val e = parseAll(expr, st); checkExtent(e.getExtent, st); e
  }

  var printParseErrors = false

  def assertParseFail(st: String) = 
    parseWith(expr, st) match{
      case Right(err) => if(printParseErrors) println(err)
      case l: Left[_,_] => println(s"Expected parse error, found $l"); sys.exit()
    }
  //  assert(parseWith(expr, st).isInstanceOf[Right[_,_]])
  
  // Parse and print the extent
  def pp(st: String) = {
    val e = parseAll(expr, st); println(s"$e.")
    println("\""+e.getExtent.asString+"\""); checkExtent(e.getExtent, st)
  }

  def env = Environment(0,0) 

  // Parse and evaluate st, and check the extent
  def pe(st: String) = {
    val v = Execution.TestHooks.eval(env, p(st))
    if(v.source == null) println(s"pe: $st -> $v")
    else checkExtent(v.source.asInstanceOf[Extent], st)
    v
  }
  
  // Parse and evaluate st; print result and source
  def pep(st: String) = { val v = pe(st); println(v); println(v.source) }

  var printErrors = false

  /** Check that v is an error, printing the error message if printErrors is
    * set. */
  def assertFail(v: Value) = v match{
    case EvalError(err) => if(printErrors) println(err)
    case _ => sys.error(s"Error expected, $v found")
  }

}
