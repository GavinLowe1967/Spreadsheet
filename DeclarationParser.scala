package spreadsheet

import Parser._

// =======================================================

object DeclarationParser extends Parser0 with DeclarationParserT{
  // Parsing of expressions.
  val expParser = new ExpParser(this)
  private val expr = expParser.expr 

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
    * "def <name>(<params>): <type> = <expr>". */
  private def funDec: Parser[FunctionDeclaration] = 
    (keyword("def") ~> name ~~ typeParams ~ inParens(params)) ~ 
      (ofType ~ (lit("=") ~> expr)) >
    { case (((n,tps),ps), (rt,e)) => FunctionDeclaration(n, tps, ps, rt, e) }
 
  /** A parser for a declaration: either a value or function declaration. */
  def declaration = valDec | funDec

  // private val outer = this

  // /** Hooks for testing. */
  // object TestHooks{
  //   //val typeP = outer.typeP 
  // }
}
