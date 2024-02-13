package spreadsheet

object StatementParser{
  import Parser._

  import ExpParser.{expr,cell}

  /** Parser for a directive, <cell> = <expr>. */
  def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > toPair(Directive) 

  def valDec: Parser[ValueDeclaration] =
    lit("val") ~> name ~ (lit("=") ~> expr) > toPair(ValueDeclaration)

  def statement: Parser[Statement] = 
    directive | valDec
  // TODO: or definitions

  def statements: Parser[List[Statement]] = statement.*

  /** Try to parse `input`, returning either the result or an error message. */
  def parseStatements(input: String): Either[List[Statement], String] = 
    parseWith(statements, input)

  def main(args: Array[String]) = {
    val vDec = "val three = 1+2"
    println(parseAll(statement, vDec))
    val dir1 = "Cell(#A, #3) = Cell(#A, #1) + Cell(#A, #2)"
    val dir2 = "#B3 = #B1 + #B2 + three" 
    println(parseAll(directive, dir1))
    println(parseAll(directive, dir2))
    println(statements(s"$dir1\n$dir2\n$vDec"))
    println(parseAll(statements, s"$dir1\n$dir2\n$vDec"))
    println(parseAll(statements, s"$vDec\n$dir1\n$dir2\n"))
  }
}
