package spreadsheet

import Parser._

/** A parser for statements. */
object StatementParser extends Parser0 with StatementParserT{
  // Parsing of expressions, cells.
  val expParser = new ExpParser(this)
  //private val expParser = DeclarationParser.expParser
  private val expr = expParser.expr
  private val cell = expParser.cell

  //import DeclarationParser.declaration


  import TypeParser.{typeP,ofType}

  /** A parser for a value declaration, "val <name> = <expr>". */
  private def valDec: Parser[ValueDeclaration] =
    keyword("val") ~> name ~ (lit("=") ~> expr) > toPair(ValueDeclaration)

  /** A parser for a list of parameters, "name1: type1, ..., namek: typek". */
  private def params: Parser[List[(String,TypeT)]] = {
    // def param: Parser[(String,TypeT)] = name ~ ofType // <~ lit(":")) ~ typeP
    repSep(name ~ ofType, ",")
  }

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

  /** A parser for a declaration: either a value or function declaration, or an
    * assertion. */
  private def declaration: Parser[Statement] = valDec | funDec | assertion

  // ===== Directives

  /** Parser for a directive, <cell> = <expr>. */
  private def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > { case ((ce,re), e) => Directive(ce,re,e) } 

  // ===== "for" expressions

  /** A parser for a single statement, or several statements in curly
    * brackets. */
  private def block: Parser[List[Statement]] = (
    statement > ((s: Statement) => List(s))
    | inBrackets(listOf(withExtent(statement)))
  )

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


  // ===== Top-level parsers

  /** A parser for a statement. */
  def statement: Parser[Statement] = withExtent(
    directive | forLoop | declaration
  )

  /** A parser for multiple statements. */
  private def statements: Parser[List[Statement]] = 
    listOf(statement) <~ atEnd

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
