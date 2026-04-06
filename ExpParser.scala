package spreadsheet

import Parser._

/** The interface of DeclarationParser, as seen by ExpParser. */
trait DeclarationParserT{
  /** A parser for a declaration. */
  def declaration: Parser[Declaration]
}


/** A parser for expressions. */
class ExpParser(declParser: DeclarationParserT) extends Parser0{

  import TypeParser.cellType

  // ===== Some basic parsers

  private val ReservedNames =
    List("Cell", "if", "else", "def", "val", "for", "match", "case", "assert",
      "true", "false", "Empty", "to", "until",
      "Int", "Float", "Boolean", "String", "Row", "Column", "List", "Eq")

  /** Parser for a name. */
  private def name1: Parser[Exp] = withExtent(
    name ? (n => !ReservedNames.contains(n)) > (n => NameExp(n)) 
  )

  /** Parser for a boolean litteral. */
  private val bool: Parser[BoolExp] = (
    keyword("true") ~~> success(BoolExp(true))
    | keyword("false") ~~> success(BoolExp(false))
  )

  /** A parser for an "if" expression. */
  private def ifP: Parser[IfExp] = withExtent(
    keyword("if") ~> inParens(expr) ~ expr ~ (keyword("else") ~> expr) > {
      case ((test, thenClause), elseClause) => 
        IfExp(test, thenClause, elseClause)
    }
  )

  // ===== Lists

  /** A parser for a list expression, either a list literal or a list
    * comprehension. */
  private def list: Parser[Exp] = 
    lit("[") ~> (
      lit("]") > (_ => ListLiteral(List[Exp]())) 
      | expr ~ list1 > { // Consume first Exp here.  
        case (e, Left(es)) => ListLiteral(e::es)
        case (e, Right(qs)) => ListComprehension(e,qs)
      }
    )

  /** A parser for either a list literal or list comprehension, following the
    * initial expression. */
  private def list1: Parser[Either[List[Exp], List[Qualifier]]] = (
    lit("]") > (_ => Left(List())) // singleton list
    | lit(",") ~> repSep(expr, ",") <~ lit("]") > (Left(_)) // 2+ elements
    | lit("|") ~> (qualifiers <~ lit("]")) > (Right(_)) // list comprehension
  )

  /** A generator in a list comprehension or "for" expression. */
  def generator = withExtent(name ~ (lit("<-") ~> expr) > toPair(Generator))

  /** A parser for a single qualifier in a list comprehension. */
  private def qualifier: Parser[Qualifier] = 
    /*withExtent*/(generator | expr > Filter)

  /** A parser for one or more qualifiers. */
  private def qualifiers: Parser[List[Qualifier]] = 
    repSepNonEmpty(qualifier, ",")

  // ===== Tuples

  // /** A parser for a tuple, with at least two elements. */
  // private def tuple: Parser[TupleLiteral] = 
  //   (lit("(") ~> expr) ~ (lit(",") ~> repSep(expr, ",") <~ lit(")")) > {
  //     case (e, es) => TupleLiteral(e::es) }

  /** A parser for a tuple with at least two elements, or an expression in
    * parentheses. */
  private def tupleOrParens: Parser[Exp] = 
    (lit("(") ~> expr) ~ (
      lit(")") ~~> success(List[Exp]()) // Note: don't consume whitespace.
      | lit(",") ~> repSep(expr, ",") <~ lit(")")
    ) > { case (e, es) => if(es.isEmpty) e else TupleLiteral(e::es) }

  // ===== Cell expressions

  /** A parser for a row or column, e.g. "#A". */
  private def rowOrColumn =
    lit("#") ~~ (int > RowExp | colName > (c => ColumnExp(c))) > { _._2 }

  /** A parser for an expression such as "#B3". */
  private def cell0: Parser[(Exp,Exp)] =
    lit("#") ~> colName ~~ posInt > 
      { case (c,r) => (ColumnExp(c), RowExp(r)) }

  /** A parser for an expression such as "Cell(B,3)" of "#B3". */
  def cell: Parser[(Exp,Exp)] = (
    keyword("Cell") ~ inParens((expr <~ lit(",")) ~ expr) > { case (x,y) => y }
      // Note: can't be interpreted as a function application
    | cell0
  )

  /** Parse the name of a column: a non-empty sequence of uppercase letters.*/
  private def colName: Parser[String] = 
    spot(_.isUpper) ~~ repeat1(spot(_.isUpper)) > 
      toPair(_::_) > (_.mkString)

  /** A parser for the latter part of a CellExp or a CellMatchExp. */
  private def cellRHS: Parser[Either[CellType, List[MatchBranch]]] = (
    lit(":") ~> cellType > { t => Left(t) }
    | 
    keyword("match") ~> inBrackets(listOf(matchBranch)) ? (_.nonEmpty) > 
      { bs => Right(bs) }
  )

  /** A parser for a pattern in a cell match expression. */
  private def pattern: Parser[Pattern] = (
    keyword("Empty") ~> success(EmptyPattern)
    | lit("_") ~> ( 
      lit(":") ~> cellType > { t => TypedPattern(None, t) } | success(Wildcard)
    )
    | name ~ (lit(":") ~> cellType) > { case (n,t) => TypedPattern(Some(n),t) }
  )

  /** A parser for a branch of a cell match expression, 
    * "case <pattern> => <expr>". */
  private def matchBranch: Parser[MatchBranch] = withExtent(
    keyword("case") ~> pattern ~ (lit("=>") ~> expr) > {
      case (p, e) => MatchBranch(p, e) 
    }
  )

  /** A parser for either a CellExp, a CellMatchExp or an UntypedCellExp. */
  private def cellExp: Parser[Exp] = 
    cell ~~ opt(Parser.consumeWhite ~~> cellRHS) > { case ((ce,re), rhs) =>  
      rhs match{
        case Some(Left(t)) => CellExp(ce, re, t)
        case Some(Right(bs)) => CellMatchExp(ce, re, bs)
        case None => UntypedCellExp(ce, re)
      }
    }
  // Note: don't consume whitespace when cellRHS fails.

  // ===== Parameters of a function.

  /** A parser for an expression that can be used as an argument of a function
    * without parentheses, and not otherwise captured within factor0. */
  private def atomicParam: Parser[Exp] = 
    string > StringExp | bool | rowOrColumn | list | block
  // Note: numbers, and cell expressions such as #A3 are captured within
  // factor0.

  /** A single parameter list, or a single unparenthesised parameter. */
  private def params: Parser[List[Exp]] = (
    inParens(repSep(expr, ","))
    | (posNum | name1 | (cell0 > toPair(UntypedCellExp)) | atomicParam) > 
      { n => List(n) }
  )
  // Notes: we include only positive numbers above; negative numbers would
  // need parentheses.  The parsers name1 and cell0 are included in more
  // general parsers within factor0, so are not included in atomicParam.

  /** Parser for arguments of a function, possibly in several sets. 
    * Note: should be sequenced to its left-hand argument using `~~`. */
  private def paramsList: Parser[List[List[Exp]]] =
    repeat1( consumeWhiteNoNL ~~> params )

  // ===== Expressions not using an infix or if at the top level. 

  /** A parser for expressions that use no infix operators or "if" statement
    * outside of parentheses, optionally with a type. */
  private def factor: Parser[Exp] = withExtent( 
    factor0 ~~ opt(consumeWhite ~~> TypeParser.ofType) > { 
      case(e, None) => e; case (e, Some(t)) => TypedExp(e,t) 
    }
  )

  // Note: for an expression such as "#A3: Int", the ": Int" is consumed by
  // factor0, specifically in cellExp, so factor produces a CellExp, as
  // opposed to a TypedExp containing an UntypedCellExp.

// IMPROVE: do we need the "withExtent" both above and below?

  /** A parser for expressions that use no infix operators or "if" statement
    * outside of parentheses. */
  private def factor0: Parser[Exp] = withExtent( 
    number | cellExp | atomicParam // | tuple 
    // Note: cellExp must precede the rowOrColumn within atomicParam 
    // Name or application of named function
    | name1 ~~ paramsList  > { case (n,args) => args.foldLeft(n)(FunctionApp) }
    // Note: in the above, inner FunctionApps don't receive an Extent.  
    // TODO: allow more general definitions of the function.
    | tupleOrParens
    //| inParens(expr) // Note: sets extent to include parentheses.
    | failure("YYY") // IMPROVE
  )

  /** A parser for a block expression. */
  private def block: Parser[BlockExp] = {
    def body: Parser[(List[Declaration], Exp)] = (
      listOf(withExtent(declParser.declaration)) ~~ (separator ~> expr)
      | expr > { e => (List(), e) }
    )
   inBrackets(body) > toPair(BlockExp)
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
    (List("+", "-"), L), (List("*", "/", "%"), L)  // 6-7
  )

  /** A parser for expressions involving binary operators of precedence at least
    * `fixity`. */
  private def infix(fixity: Int): Parser[Exp] = (
    if(fixity == operators.length) factor
    else{
      // A parser for the operator `op`.  Note: we need to treat "word"
      // operators differently from special-character operators.
      def pp(op: String) = if(op.head.isLetter) keyword(op) else lit(op)
      // Parser for an "op term" subexpression, where term uses operators of
      // higher precedence.  Note: consumes white space at the start.
      def p0(op: String) = consumeWhite ~> pp(op) ~ infix(fixity+1)
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
