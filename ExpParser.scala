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
  def expr: Parser[Exp] = withExtent(infix(0))

  /** A parser for expressions involving binary operators of precedence at least
    * `fixity`. */
  private def infix(fixity: Int): Parser[Exp] = withExtent(
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


/*
    else{
      def p0(op: String) = consumeWhite ~> lit(op) ~ infix(fixity)
      val p1 = | ( for(op <- operators(fixity)) yield p0(op) )

      infix(fixity+1) ~~ opt(p1) > { 
        case (e, None) => e; case (e1, Some((op,e2))) => BinOp(e1, op, e2)
      }
    }
 */
    // else(
    //   infix(fixity+1) ~
    //     ( | (for(op <- operators(fixity)) yield lit(op) ~ infix(fixity+1))
    //     ) .* > { toPair(mkBin) }
    // )

  /** Convert f and ofs into an Exp, by associating to the left. */
  private def mkBin(f: Exp, ofs: List[(String, Exp)]): Exp = 
    if(ofs.isEmpty) f 
    else{ 
      val (op, f1) = ofs.head; val term1 = BinOp(f, op, f1)
      // Set the extent of the first term
      term1.setExtent(f.getExtent.until(f1.getExtent).asInstanceOf[Extent])
// IMPROVE
      mkBin(term1, ofs.tail)
      // mkBin(BinOp(f, op, f1), ofs.tail)
    }

  /** A parser for expressions that use no infix operators outside of
    * parentheses: atomic terms and parenthesised expressions. */
  private def factor: Parser[Exp] = (
    int > IntExp
    | name > NameExp
    | cell1
    | lit("#") ~> hashTerm 
    | inParens(expr) 
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
    // Parse as expression
    def p(st: String) = parseAll(expr, st)

    // Parse and print the extent
    def pp(st: String) = {
      val e = parseAll(expr, st); println(e); 
      println("\""+e.getExtent.asString+"\"")
    }
    // pp("123"); pp("2*3")
    // pp("(2+3)*4 <= 6 && 6*7 ==   42")
    // pp("  2 + 2 + two  ")
    // pp("  2    ")
    // println(int(new Input("2  ")))
    // println(expr(new Input("2 + 2    ")))

    assert(p("123") == IntExp(123))
    assert(p(" ( -123 ) ") == IntExp(-123))
    assert(p("foo") == NameExp("foo"))
    assert(p(" ( foo ) ") == NameExp("foo"))

    assert(p("2+3") == BinOp(IntExp(2), "+", IntExp(3)))
    assert(p("2+-3") == BinOp(IntExp(2), "+", IntExp(-3)))
    assert(p("2+3-4") == BinOp(BinOp(IntExp(2), "+", IntExp(3)), "-", IntExp(4)))

    val env = new Environment(null)
    // Parse and evaluate st
    def pe(st: String) = p(st).eval(env)

    def pep(st: String) = {
      val v = pe(st); println(v); println(v.source)
    }
    // pep("2+3"); pep("(2==3) + (4==5)")
    pep("2 + 3")
    pep("2 + 3 + 4")

    // println(pe("2 + (3 == 4)"))
    assert(pe("2+3*4") == IntValue(14))
    assert(pe("2*3+4") == IntValue(10))
    assert(pe("(2+3)*4 == 6") == BoolValue(false))
    assert(pe("(2+3)*4 == 60/3") == BoolValue(true))
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
