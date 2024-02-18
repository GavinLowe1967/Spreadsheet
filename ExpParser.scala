package spreadsheet

/** A general parser for expressions. */
object ExpParser{
  import Parser._

  /** Lift `p`, so its result is annotated with its extent. */
  def withExtent(p: => Parser[Exp]) = new Parser[Exp]{
    def apply(in: Input) = p(in) match{
      case s @ Success(exp, in1) => exp.setExtent(in.until(in1)); s
      case failure => failure
    }
  }

  /** Binary operators, in increasing order of precedence.  */
  private val operators = Array(
    List("||"), List("&&"), // 2-3 in Haskell
    List("==", "!=", "<", "<=", ">", ">="), // 4
    List("+", "-"), List("*", "/")  // 6-7
  )

  /** A parser for expressions. */
  def expr: Parser[Exp] = infix(0)

  /** A parser for expressions involving binary operators of precedence at least
    * `fixity`. */
  private def infix(fixity: Int): Parser[Exp] = /*withExtent*/(
    if(fixity == operators.length) factor
    else{
      // Parser for an "op term" subexpression, where term uses operators of
      // higher precedence.  Note: consumes white space at the start.
      def p0(op: String) = consumeWhite ~> lit(op) ~ infix(fixity+1)
      // Parser for all operators of this precedence. 
      val p1 = | ( for(op <- operators(fixity)) yield p0(op) )
      // And repeat
      infix(fixity+1) ~~ repeat1(p1) > { toPair(mkBin) }
      // Note: above is designed not to consume training white space. 
    }
  )

  /** Convert f and ofs into an Exp, by associating to the left. */
  private def mkBin(f: Exp, ofs: List[(String, Exp)]): Exp = 
    if(ofs.isEmpty) f 
    else{ 
      val (op, f1) = ofs.head; val term1 = BinOp(f, op, f1)
      // Set the extent of the first term -- done by BinOp constructor.
      // term1.setExtent(f.getExtent.until(f1.getExtent))
      mkBin(term1, ofs.tail)
    }

/*    def p0(op: String) = consumeWhite ~> lit(op) ~ infix(fixity)
      val p1 = | ( for(op <- operators(fixity)) yield p0(op) )
      infix(fixity+1) ~~ opt(p1) > { 
        case (e, None) => e; case (e1, Some((op,e2))) => BinOp(e1, op, e2)
 */
    // else(
    //   infix(fixity+1) ~
    //     ( | (for(op <- operators(fixity)) yield lit(op) ~ infix(fixity+1))
    //     ) .* > { toPair(mkBin) }
    // )


  /** A parser for expressions that use no infix operators outside of
    * parentheses: atomic terms and parenthesised expressions. */
  private def factor: Parser[Exp] = withExtent( 
    int > IntExp
    | name > NameExp
    | cell1
    | lit("#") ~> hashTerm  
    | inParens(expr) // Note: sets extent to include parentheses.
       // TO DO: add function application, ...
  )

  /** Parse a subexpression folling a "#". */
  private def hashTerm: Parser[Exp] = (
    int > RowExp
    | ((colName > ColumnExp) ~ opt(int)) > 
      { case (ce,None) => ce; case (ce,Some(r)) => CellExp(ce, RowExp(r)) }
  )

  /** Parse a reference to a Cell, a string of the form "Cell([...], [...])". */
  private def cell1: Parser[CellExp] = 
    ( lit("Cell") ~> inParens((expr <~ lit(",")) ~ expr) ) >
      toPair(CellExp) 

  /** Parse a reference to a Cell, either of the form Cell(#C,#3) or #C3. */
  def cell: Parser[CellExp] = (
    cell1
    | lit("#") ~> colName ~ int > 
      { case (c,r) => CellExp(ColumnExp(c), RowExp(r)) }
  )

  /** Parse the name of a column: a non-empty sequence of uppercase letters.*/
  private def colName: Parser[String] = 
    spot(_.isUpper) ~ repeat1(spot(_.isUpper)) > 
      toPair(_::_) > (_.mkString)

  /** Parser for a value entered in a cell by the user. */
  private def userValue: Parser[Cell] = (
    (int <~ atEnd) > IntValue 
    | all > StringValue
  )

  def parseUserValue(st: String): Cell = 
    parseAll(userValue, st)


  // =======================================================

  def main(args: Array[String]) = {
    def isWhite(c: Char) = c == ' ' || c == '\t' || c == '\n'
    // Remove leading and training whilespace from st
    def trim(st: String) =
      st.dropWhile(isWhite).reverse.dropWhile(isWhite).reverse
    // Check that ext matches st
    def checkExtent(ext: Extent, st: String) = {
      val st1 = trim(st); val st2 = ext.asString
      assert(st2 == st1, s"\"$st2\" != \"$st1\"")
    }

    // Parse as expression
    def p0(st: String): Exp = parseAll(expr, st)
    // Parse as expression, and check extent
    def p(st: String): Exp = {
      val e = parseAll(expr, st); checkExtent(e.getExtent, st); e
    }
    // Parse and print the extent
    def pp(st: String) = {
      val e = parseAll(expr, st); println(e); checkExtent(e.getExtent, st)
      println("\""+e.getExtent.asString+"\"")
    }

    assert(p("123") == IntExp(123))
    assert(p(" ( -123 ) ") == IntExp(-123))
    assert(p("foo") == NameExp("foo"))
    assert(p(" ( foo ) ") == NameExp("foo"))

    assert(p("2+3") == BinOp(IntExp(2), "+", IntExp(3)))
    assert(p("2+-3") == BinOp(IntExp(2), "+", IntExp(-3)))
    assert(p("2+3-4") == BinOp(BinOp(IntExp(2), "+", IntExp(3)), "-", IntExp(4)))

    val env = new Environment(null)

    // Parse and evaluate st
    def pe(st: String) = {
      val v = p(st).eval(env)
      if(v.source == null) println(s"$st -> $v")
      checkExtent(v.source.asInstanceOf[Extent], st)
      v
    }
    // Parse and evaluate st; print result and source
    def pep(st: String) = {
      val v = pe(st); println(v); println(v.source); 
    }
    pe("2 + 3"); pe("2 + 3 + 4")

    assert(pe("2+3*4") == IntValue(14))
    assert(pe("2*3+4") == IntValue(10))
    assert(pe("(2+3)") == IntValue(5))
    assert(pe("(2+3)*4 == 6") == BoolValue(false))
    assert(pe("(1+4)*4 == 60/3") == BoolValue(true))
    assert(pe("(2+3)*4 != 6") == BoolValue(true))
    assert(pe("(2+3)*4 != 60/3") == BoolValue(false))
    assert(pe("(2+3)*4 > 6") == BoolValue(true))
    assert(pe("(2+3)*4 <= 6 || 6*7 == 42") == BoolValue(true))
    assert(pe("(2+3)*4 <= 6 && 6*7 == 42") == BoolValue(false))

    assert(pe("#23") == RowValue(23))
    assert(pe("#Z") == ColumnValue(25)); assert(pe("#AB") == ColumnValue(27))
    // Is the following what we want?? 
    assert(expr("#Aa").asInstanceOf[Success[Exp]].result == ColumnExp("A"))
    assert(expr("#a").isInstanceOf[Failure])

    //println(p("Cell(#A, #2)"))
    assert(p("Cell(#HW, #23)") == CellExp(ColumnExp("HW"), RowExp(23)))

    // println(p("#A2 + #A3"))

    println("Done")
  }

}
