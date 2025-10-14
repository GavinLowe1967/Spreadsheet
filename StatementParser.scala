package spreadsheet

/* Note: ExpParser and StatementParser are mutually recursive, so they're
 * defined in the same file. */

import Parser._

/** A parser for expressions. */
object ExpParser extends Parser0{
  /** Lift `p`, so its result is annotated with its extent. */
  def withExtent[A <: HasExtent](p: => Parser[A]) = new Parser[A]{
    def apply(in: Input) = p(in) match{
      case s @ Success(exp, in1) => exp.setExtent(in.until(in1)); s
      case failure => failure
    }
  }

  // ===== Some basic parsers

  private val ReservedNames =
    List("Cell", "if", "else", "def", "val", "for", "true", "false")

  /** Parser for a name. */
  private def name1: Parser[Exp] = withExtent(
    name ? (n => !ReservedNames.contains(n)) > (n => NameExp(n)) 
  )

  /** Parser for a boolean litteral. */
  private val bool: Parser[BoolExp] = (
    lit("true") ~~> success(BoolExp(true))
    | lit("false") ~~> success(BoolExp(false))
  )

  /** A parser for an "if" expression. */
  private def ifP: Parser[IfExp] = withExtent(
    lit("if") ~> inParens(expr) ~ expr ~ (lit("else") ~> expr) > {
      case ((test, thenClause), elseClause) => 
        IfExp(test, thenClause, elseClause)
    }
  )

  /** Parser for the arguments of a function, with no preceding newline.
    * Note: should be sequenced to its left-hand argument using `~~`. */
  private def params: Parser[Option[List[Exp]]] =
    opt( consumeWhiteNoNL ~~ inParens(repSep(expr, ",")) > (_._2))

  /** A parser for a list expression. */
  private def list: Parser[ListLiteral] = (
    (lit("[") ~> repSep(expr, ",")) <~ lit("]") > ListLiteral
  )

  // ===== Cell expressions

  /** A parser for an expression such as "Cell(B,3)" of "#B3". */
  def cell: Parser[(Exp,Exp)] = (
    lit("Cell") ~! inParens((expr <~ lit(",")) ~ expr) > { case (x,y) => y }
    // Don't allow backtracking, or this could be parsed as a function
    // application.
    | lit("#") ~> colName ~~ posInt > 
      { case (c,r) => (ColumnExp(c), RowExp(r)) }
  )

  /** Parse the name of a column: a non-empty sequence of uppercase letters.*/
  private def colName: Parser[String] = 
    spot(_.isUpper) ~~ repeat1(spot(_.isUpper)) > 
      toPair(_::_) > (_.mkString)

  /** A parser for the latter part of a CellExp or a CellMatchExp. */
  private def cellRHS: Parser[Either[CellType, List[MatchBranch]]] = (
    lit(":") ~> cellType > { t => Left(t) }
    | 
    lit("match") ~> lit("{") ~> repeatUntil(matchBranch, separator, lit("}")) > 
      { case(bs,_) => Right(bs) }
  )

  /** A parser for a CellType. */
  def cellType: Parser[CellType] = (
    lit("Int") > { _ => IntType } | lit("Float") > { _ => FloatType }
    | lit("Boolean") > { _ => BoolType } | lit("String") > { _ => StringType }
  )

  /** A parser for a pattern in a cell match expression. */
  private def pattern: Parser[Pattern] = (
    lit("Empty") ~> success(EmptyPattern)
    | lit("_") ~> ( 
      lit(":") ~> cellType > { t => TypedPattern(None, t) } | success(Wildcard)
    )
    | name ~ (lit(":") ~> cellType) > { case (n,t) => TypedPattern(Some(n),t) }
  )

  /** A parser for a branch of a cell match expression, 
    * "case <pattern> => <expr>". */
  private def matchBranch: Parser[MatchBranch] = withExtent(
    lit("case ") ~> pattern ~ (lit("=>") ~> expr) > {
      case (p, e) => MatchBranch(p, e) 
    }
  )

  /** A parser for either a CellExp, a CellMatchExp or an UntypedCellExp. */
  private def cellExp: Parser[Exp] = 
    cell ~~! opt(Parser.consumeWhite ~~> cellRHS) > { case ((ce,re), rhs) =>  
      rhs match{
        case Some(Left(t)) => CellExp(ce, re, t)
        case Some(Right(bs)) => CellMatchExp(ce, re, bs)
        case None => UntypedCellExp(ce, re)
      }
    }
  // Note: don't consume whitespace when cellRHS fails.  Don't allow
  // backtracking above, or "#A1" gets passed as "#A" with the extra lost,


  // ===== Expressions not using an infix or if at the top level. 

  /** A parser for expressions that use no infix operators or "if" statement
    * outside of parentheses. */
  private def factor: Parser[Exp] = withExtent( 
    number
    | string > StringExp | cellExp | bool
    // Name or application of named function
    | name1 ~~ params > {
      case (n, None) => n; case (n, Some(ps)) => FunctionApp(n, ps)
    }
    // TODO: allow more general definitions of the function.
      // Row and column literals
    | lit("#") ~~ (int > RowExp | colName > ColumnExp) > { _._2 }  
    | inParens(expr) // Note: sets extent to include parentheses.
    | list | block
    | failure("YYY") // IMPROVE
  )

  /** A parser for a block expression. */
  private def block: Parser[BlockExp] = {
    import StatementParser.{listOf,declaration}
    def body: Parser[(List[Declaration], Exp)] = (
      repeat1(withExtent(consumeWhite ~> declaration) <~~ separator) ~ expr 
      //listOf(withExtent(declaration)) ~~ (separator ~> expr)
      //| expr > { e => (List[Declaration](), e) }
    )
    (lit("{") ~> body) <~ lit("}") > toPair(BlockExp)
  }

  // ===== Infix operators

  /** Representations of left- or right-associativity for operators. */
  private val L = 0; private val R = 1

  /** Binary operators, in increasing order of precedence.  Each is tagged with
    * L or R to indicate associativity. */
  private val operators = Array(
    (List("to", "until"), L),
    (List("||"), R), (List("&&"), R), // 2-3 in Haskell 
    (List("==", "!=", "<", "<=", ">", ">="), L), // 4
    (List("::"), R), // 5
    (List("+", "-"), L), (List("*", "/"), L)  // 6-7
  )

  /** A parser for expressions involving binary operators of precedence at least
    * `fixity`. */
  private def infix(fixity: Int): Parser[Exp] = (
    if(fixity == operators.length) factor
    else{
      // Parser for an "op term" subexpression, where term uses operators of
      // higher precedence.  Note: consumes white space at the start.
      def p0(op: String) = consumeWhite ~> lit(op) ~ infix(fixity+1)
      val (ops,dir) = operators(fixity)
      // Parser for all operators of this precedence. 
      val p1 = | ( for(op <- ops) yield p0(op) )
      // And repeat
      infix(fixity+1) ~~ repeat1(p1) > { 
        toPair(if(dir == L) mkBinL else mkBinR) 
      }
      // Note: above is designed not to consume trailing white space. 
    }
  )

  /** Convert f and ofs into an Exp, by associating to the left. */
  private def mkBinL(f: Exp, ofs: List[(String, Exp)]): Exp = 
    if(ofs.isEmpty) f 
    else{ val (op, f1) = ofs.head; mkBinL(BinOp(f, op, f1), ofs.tail) }

  /** Convert f and ofs into an Exp, by associating to the right. */
  private def mkBinR(f: Exp, ofs: List[(String, Exp)]): Exp = 
    if(ofs.isEmpty) f 
    else{ val (op,f1) = ofs.head; BinOp(f, op, mkBinR(f1, ofs.tail)) }

  // ===== Top-level parser.

  /** A parser for expressions. */
  def expr: Parser[Exp] = ifP | infix(0)
}

// =======================================================

object StatementParser extends Parser0{
  import ExpParser.{expr,cell,withExtent}

  /** Parser for a directive, <cell> = <expr>. */
  private def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > { case ((ce,re), e) => Directive(ce,re,e) } 

  /** A parser for a value declaration, "val <name> = <expr>". */
  private def valDec: Parser[ValueDeclaration] =
    lit("val") ~> name ~ (lit("=") ~> expr) > toPair(ValueDeclaration)

  /** Parser for an expression in square brackets. */
  private def inSquare[A](p: Parser[A]): Parser[A] = 
    (lit("[") ~> p) <~ lit("]")

  /** A parser for a type name or type parameter. */
  private def typeP1: Parser[TypeT] = (
    ExpParser.cellType
    | lit("Row") > { _ => RowType } | lit("Column") > { _ => ColumnType }
    | lit("List") ~> inSquare(typeP) > { t => ListType(t) }
    | upperName > { n => TypeParam(n) }
  )

  /** A parser for a type. */
  def typeP: Parser[TypeT] = 
    typeP1 ~ opt(lit("=>") ~> typeP) > { _ match {
      case (t,None) => t
      case (t1, Some(t2)) => FunctionType(List(), List(t1), t2)
        // IMPROVE: the "List()" looks odd.
    }}

  /** A parser for a list of parameters, "name1: type1, ..., namek: typek". */
  private def params: Parser[List[(String,TypeT)]] = {
    def param: Parser[(String,TypeT)] = (name <~ lit(":")) ~ typeP
    repSep(param, ",")
  }

  import FunctionType.TypeParameter

  private def typeConstraint: Parser[TypeParamConstraint] = (
    lit("<:") ~> (
      lit ("Eq") > (_ => EqTypeConstraint) 
      // | lit("Num") > (_ => NumTypeConstraint) // MemberOf(TypeT.NumTypes))
    ) 
    | success(AnyTypeConstraint)
  )

  /** Parser for a single type parameter. */
  private  def typeParam: Parser[TypeParameter] =
    upperName ~ typeConstraint

  /** Parser for type parameters. */
  private def typeParams: Parser[List[TypeParameter]] = 
    opt(inSquare(repSep(typeParam, ","))) > 
      { case Some(ts) => ts; case None => List() }

  /** A parser for a function declaration 
    * "def <name>(<params>): <type> = <expr>". */
  def funDec: Parser[FunctionDeclaration] = 
    (lit("def") ~> name ~ typeParams ~ inParens(params)) ~ 
      ((lit(":") ~> typeP) ~ (lit("=") ~> expr)) >
    { case (((n,tps),ps), (rt,e)) => FunctionDeclaration(n, tps, ps, rt, e) }
 
  /** A parser for a declaration: either a value or function declaration. */
  def declaration = valDec | funDec

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
    | lit("{") ~> listOf(withExtent(statement)) <~ lit("}")
  )

  /** A parser for a "for" statement. */
  private def forLoop: Parser[ForStatement] = 
    (lit("for") ~> binders ~ block) > toPair(ForStatement) 

  /** A parser for a statement. */
  def statement: Parser[Statement] = withExtent(
    directive | forLoop | declaration
  )


  /** A parser running `p` repeatedly, zero or more times. */
  def listOf[A <: Statement](p: Parser[A]): Parser[List[A]] = 
    //repSep(p, separator)
    p ~~ repeat1(separator ~> p) > toPair(_::_) | success(List())

  /** A parser for one or more statements. */
  def statements: Parser[List[Statement]] = 
    repeatUntil(statement, separator, atEnd) > (_._1)

  /** Try to parse `input`, returning either the result or an error message. */
  def parseStatements(input: String): Either[List[Statement], String] = {
    parseWith(statements, input)
  }

}
