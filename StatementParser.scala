package spreadsheet

import Parser._

/** A parser for statements. */
object StatementParser extends Parser0{
  import ExpParser.{expr,cell}
  import DeclarationParser.declaration

  /** Parser for a directive, <cell> = <expr>. */
  private def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > { case ((ce,re), e) => Directive(ce,re,e) } 

  // ===== "for" expressions

  /** A parser for a single binder in a "for" expression. */
  private def binder: Parser[Binder] = (
    name ~ (lit("<-") ~> expr) > toPair(Generator) 
    | lit("if") ~> expr > Filter
  )

  /** A parser for one or more binders, in parentheses. */
  private def binders: Parser[List[Binder]] = 
    inParens(repSepNonEmpty(binder, ";"))

  /** A parser for a single statement, or several statements in curly
    * brackets. */
  private def block: Parser[List[Statement]] = (
    statement > ((s: Statement) => List(s))
    | inBrackets(listOf(withExtent(statement)))
  )

  /** A parser for a "for" statement. */
  private def forLoop: Parser[ForStatement] = 
    (lit("for") ~> binders ~ block) > toPair(ForStatement) 

  // Top-level parsers

  /** A parser for a statement. */
  private def statement: Parser[Statement] = withExtent(
    directive | forLoop | declaration
  )

  /** A parser for multiple statements. */
  private def statements: Parser[List[Statement]] = 
    listOf(statement) <~ atEnd
//    repeatUntil(statement, separator, atEnd) > (_._1)

  /** Try to parse `input`, returning either the result or an error message. */
  def parseStatements(input: String): Either[List[Statement], String] = {
    parseWith(statements, input)
  }

  private val outer = this

  /** Hooks for testing. */
  object TestHooks{
    val statement = outer.statement
    val statements = outer.statements
  }

}
