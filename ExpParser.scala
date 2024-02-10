package spreadsheet

/** A general parser for expressions. */
object ExpParser{
  import Parser._

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
  private def infix(fixity: Int): Parser[Exp] = 
    if(fixity == operators.length) factor
    else(
      infix(fixity+1) ~
        ( | (for(op <- operators(fixity)) yield lit(op) ~ infix(fixity+1))
        ) .* > { toPair(mkBin) }
        // { case (f, ofs) => mkBin(f,ofs) }
    )

  /** Convert f and ofs into an Exp, by associating to the left. */
  private def mkBin(f: Exp, ofs: List[(String, Exp)]): Exp = 
    if(ofs.isEmpty) f 
    else{ val (op, f1) = ofs.head; mkBin(BinOp(f, op, f1), ofs.tail) }

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
  private def userValue: Parser[UserValue] = (
    (int <~ atEnd) > IntValue 
    | all > StringValue
  )

  def parseUserValue(st: String): UserValue = 
    parseAll(userValue, st)


  // =======================================================

  def main(args: Array[String]) = {
    // Parse as expression
    def p(st: String) = parseAll(expr, st)

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

    println(p("#A2 + #A3"))

    println("Done")
  }

}
