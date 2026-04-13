package spreadsheet

import Parser._

/** A parser for statements. */
object StatementParser extends Parser0 with StatementParserT{
  val expParser = new ExpParser(this)
  private val expr = expParser.expr
  private val cell = expParser.cell

  import TypeParser.{typeP,ofType}

  // ===== val declarations

  /** A parser for a value declaration, "val <name> = <expr>". */
  private def valDec: Parser[ValueDeclaration] =
    keyword("val") ~> name ~ (lit("=") ~> expr) > toPair(ValueDeclaration)

  // ===== def declarations

  /** A parser for a list of parameters, "name1: type1, ..., namek: typek". */
  private def params: Parser[List[(String,TypeT)]] = repSep(name ~ ofType, ",")

  import FunctionType.TypeParameter

  private def typeConstraint: Parser[TypeParamConstraint] = (
    lit("<:") ~> (
      keyword("Eq") > (_ => EqTypeConstraint) 
      // | lit("Num") > (_ => NumTypeConstraint) // MemberOf(TypeT.NumTypes))
    ) 
    | success(AnyTypeConstraint)
  )

  /** Parser for a single type parameter. */
  private  def typeParam: Parser[TypeParameter] =
    upperName ~ typeConstraint

  /** Parser for type parameters. */
  private def typeParams: Parser[List[TypeParameter]] = 
    opt(consumeWhite ~~> inSquare(repSep(typeParam, ","))) > 
      { case Some(ts) => ts; case None => List() }

  /** A parser for a function declaration 
    * "def <name>(<params>): <type> = <expr>", where the ": <type>" is 
    * optional. */
  private def funDec: Parser[FunctionDeclaration] = 
    (keyword("def") ~> name ~~ typeParams ~ inParens(params).+) ~ 
      (opt(ofType) ~ (lit("=") ~> expr)) >
    { case (((n,tps),ps), (ort,e)) => FunctionDeclaration(n, tps, ps, ort, e) }

  // ===== Assertions

  /** A parser for an assertion. */
  private def assertion: Parser[Statement] = (
    (keyword("assert") ~~ lit("(") ~> expr) ~~ 
      (opt(lit(",") ~> expr) <~ lit(")")) 
      > { case (e, None) => Assertion(e); case (e, Some(m)) => Assertion2(e,m) }
  )

  // ===== Directives

  /** Parser for a directive, <cell> = <expr>. */
  private def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > { case ((ce,re), e) => Directive(ce,re,e) } 

  // ===== "for" expressions

  /** A parser for a single statement, or several statements in curly
    * brackets. */
  private def block: Parser[List[Statement]] = (
    inBrackets(listOf(withExtent(statement)))
    | statement > ((s: Statement) => List(s))
  )
  // Note: the order is important, of else "{#A1 = 3}" gets parsed as
  // List( CallStmt( BlockExp( List(Directive(...)), null ) ) ).

  /** A parser for a single qualifier in a "for" expression. */
  private def qualifier: Parser[Qualifier] = (
    expParser.generator | keyword("if") ~> expr > Filter
  )

  /** A parser for one or more qualifiers in parentheses. */
  private def qualifiers: Parser[List[Qualifier]] = 
    inParens(repSepNonEmpty(qualifier, ";"))

  /** A parser for a "for" statement. */
  private def forLoop: Parser[ForStatement] = 
    keyword("for") ~> (qualifiers ~ block) > toPair(ForStatement)

  // ===== "call" statements

  /** Parser for a CallStmt. */
  private def callStmt: Parser[CallStatement] = expr > CallStatement
  /* keyword("call") ~>*/

  // ===== Top-level parsers

  /** A parser for a statement. */
  def statement: Parser[Statement] = withExtent(
    valDec | funDec | assertion | directive | forLoop | callStmt
  )

  /** A parser for multiple statements. */
  private def statements: Parser[List[Statement]] = listOf(statement) <~ atEnd

  // /** Try to parse `input`, returning either the result or an error message. */
  // def parseStatements(input: String): Either[List[Statement], String] = 
  //   parseWith(statements, input)

  /** Try to parse `input`, returning either the result or an error message. */
  def parseStatements(input: Input): Either[List[Statement], String] = 
    parseWith(statements, input)

  // ===== Test hooks

  private val outer = this

  /** Hooks for testing. */
  object TestHooks{
    val statement = outer.statement
    val statements = outer.statements
  }
}
