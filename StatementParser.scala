package spreadsheet

/* Note: ExpParser and StatementParser are mutually exclusive, so they're
 * defined in the same file. */

import Parser._

/** A general parser for expressions. */
object ExpParser{

  /** Lift `p`, so its result is annotated with its extent. */
  def withExtent[A <: HasExtent](p: => Parser[A]) = new Parser[A]{
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
  private def infix(fixity: Int): Parser[Exp] = (
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
    ( lit("Cell") ~> inParens((expr <~ lit(",")) ~ expr) ) > toPair(CellExp) 

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

  /** Parse a value input into a cell.  Called by Spreadsheet. */
  def parseUserValue(st: String): Cell = parseAll(userValue, st)

  // =====================================

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

    val env = new Environment(null, null)

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

// =======================================================

object StatementParser{
  import ExpParser.{expr,cell,withExtent}

  /** Parser for a directive, <cell> = <expr>. */
  private def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > toPair(Directive) 

  private def valDec: Parser[ValueDeclaration] =
    lit("val") ~> name ~ (lit("=") ~> expr) > toPair(ValueDeclaration)

  /** A parser for a separator between statements, either a newline or a
    * semicolon.  Note: this consumes white space at the start of its input:
    * it should be sequenced with the preceding parser using `~~`. */
  private def separator: Parser[String] = toLineEnd | consumeWhite ~> lit(";") 

  private def statement: Parser[Statement] = withExtent(
    directive | valDec
  )
  // TODO: or definitions

  /** A parser for one or more statements. */
  def statements: Parser[List[Statement]] = 
    statement ~~ repeat1(separator ~> statement) > toPair(_::_)

  /** Try to parse `input`, returning either the result or an error message. */
  def parseStatements(input: String): Either[List[Statement], String] = {
    val stmts = parseWith(statements, input)
    stmts match{ 
      case Left(ss) => println(ss.mkString("\n"))
      case Right(msg) => println(msg)
    }
    stmts
  }

  def main(args: Array[String]) = {
    val vDec = "val three = 1+2"
    val vDecR = ValueDeclaration("three", BinOp(IntExp(1), "+", IntExp(2)))
    // println(parseAll(statement, vDec))
    assert(parseAll(statement, vDec) == vDecR)

    def cellExp(c: String, r: Int) = CellExp(ColumnExp(c), RowExp(r))
    val dir1 = "Cell(#A, #3) = Cell(#A, #1) + Cell(#A, #2)"
    val dir1R = 
      Directive(cellExp("A",3), BinOp(cellExp("A",1), "+", cellExp("A",2)))
    // println(parseAll(directive, dir1))
    assert(parseAll(directive, dir1) == dir1R)

    val dir2 = "#B3 = #B1 + #B2 - three" 
    val dir2R = Directive(
      cellExp("B",3),
      BinOp( BinOp(cellExp("B",1), "+", cellExp("B",2)),  "-", NameExp("three") )
    )
    assert(parseAll(directive, dir2) == dir2R)
    // println(parseAll(directive, dir2))

    assert(parseAll(statements, s"$dir1\n$dir2\n$vDec") == 
      List(dir1R, dir2R, vDecR) )
    // println(statements(s"$dir1\n$dir2\n$vDec"))
    // println(parseAll(statements, s"$dir1\n$dir2\n$vDec"))

    assert(parseAll(statements, s"$vDec;$dir1;$dir2") ==
      List(vDecR, dir1R, dir2R))
    // println(parseAll(statements, s"$vDec\n$dir1\n$dir2\n"))
  }
}
