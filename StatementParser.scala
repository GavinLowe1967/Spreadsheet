package spreadsheet

/* Note: ExpParser and StatementParser are mutually recursive, so they're
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

  /** Representations of left- or right-associativity for operators. */
  private val L = 0; private val R = 1

  /** Binary operators, in increasing order of precedence.  Each is tagged with
    * L or R to indicate associativity. */
  private val operators = Array(
    (List("||"), L), (List("&&"), L), // 2-3 in Haskell  *** OR R?
    (List("==", "!=", "<", "<=", ">", ">="), L), // 4
    (List("::"), R), // 5
    (List("+", "-"), L), (List("*", "/"), L)  // 6-7
  )

  /** A parser for an "if" expression. */
  private def ifP: Parser[IfExp] = withExtent(
    lit("if") ~> inParens(expr) ~ expr ~ (lit("else") ~> expr) > {
      case ((test, thenClause), elseClause) => 
        IfExp(test, thenClause, elseClause)
    }
  )

  /** A parser for expressions. */
  def expr: Parser[Exp] = ifP | infix(0)

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
    else{ 
      val (op, f1) = ofs.head; val term1 = BinOp(f, op, f1)
      // Set the extent of the first term -- done by BinOp constructor.
      // term1.setExtent(f.getExtent.until(f1.getExtent))
      mkBinL(term1, ofs.tail)
    }

  /** Convert f and ofs into an Exp, by associating to the right. */
  private def mkBinR(f: Exp, ofs: List[(String, Exp)]): Exp = 
    if(ofs.isEmpty) f 
    else{ val (op,f1) = ofs.head; BinOp(f, op, mkBinR(f1, ofs.tail)) }

  /** Parser for the arguments of a function, with no preceding newline.
    * Note: should be sequenced to its left-hand argument using `~~`. */
  private def params: Parser[Option[List[Exp]]] =
    opt( consumeWhiteNoNL ~~ inParens(repSep(expr, ",")) > (_._2))

  /** A name, translating reserved names into their values. */
  def name1: Parser[Exp] = withExtent(
    name > {
      case "true" => BoolExp(true)
      case "false" => BoolExp(false)
      case n => NameExp(n)
    }
  )

  /** A parser for expressions that use no infix operators outside of
    * parentheses: atomic terms and parenthesised expressions. */
  private def factor: Parser[Exp] = withExtent( 
    number // int > IntExp
    // Name or application of named function 
    | name1 ~~ params > {
      case (n, None) => n
      case (n, Some(ps)) => FunctionApp(n, ps)
    }
    // TODO: allow more general definitions of the function.  
    | cell1
    | lit("#") ~~ hashTerm > { _._2 }  
    | inParens(expr) // Note: sets extent to include parentheses.
    | list
    | block
  )

  /** A parser for an Int or Float. */
  private def number: Parser[Exp] = {
    /* Convert `ds` to an Int. */
    def mkInt(ds: List[Char]): Int = {
      var ds1 = ds; var x = 0
      while(ds1.nonEmpty){ x = 10*x+(ds1.head-'0'); ds1 = ds1.tail }
      x
    }
    /* Convert `ds` to the Float represented by `0.ds`. */
    def mkFloat(ds: List[Char]): Float = {
      var ds1 = ds.reverse; var x = 0.0F
      while(ds1.nonEmpty){ x = (x+ds1.head-'0')/10.0F; ds1 = ds1.tail }
      x
    }
    // Parser for repeated digits
    val digits: Parser[List[Char]] = 
      spot(_.isDigit) ~~ repeat1(spot(_.isDigit)) > toPair(_::_)
    // Parser for positive numbers
    val posNum: Parser[Exp] = 
      digits ~~ opt(lit(".") ~~ digits) > { 
        case (intPart, None) => IntExp(mkInt(intPart))
        case (intPart, Some((_,fracPart))) => 
          FloatExp(mkInt(intPart) + mkFloat(fracPart))
      }
    // Main parser: allow leading "-"
    lit("-") ~~ posNum > { 
      case (_,IntExp(n)) => IntExp(-n); case (_,FloatExp(x)) => FloatExp(-x) 
    } |
    posNum
  }


  import StatementParser.{listOf,declaration,separator}

  /** A parser for a block expression. */
  private def block: Parser[BlockExp] = {
    def body: Parser[(List[Declaration], Exp)] = (
      listOf(withExtent(declaration)) ~~ (separator ~> expr)
      | expr > { e => (List[Declaration](), e) }
    )
    (lit("{") ~> body) <~ lit("}") > toPair(BlockExp)
  }

  /** A parser for a list expression. */
  private def list: Parser[ListLiteral] = (
    (lit("[") ~> repSep(expr, ",")) <~ lit("]") > ListLiteral
  )

  /** Parse a subexpression folling a "#". */
  private def hashTerm: Parser[Exp] = (
    int > RowExp
    | ((colName > ColumnExp) ~~ opt(int)) > 
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
    spot(_.isUpper) ~~ repeat1(spot(_.isUpper)) > 
      toPair(_::_) > (_.mkString)

  /** Parser for a value entered in a cell by the user. */
  private def userValue: Parser[Cell] = (
    // (int <~ atEnd) > IntValue 
    (number <~ atEnd) > { 
      case IntExp(n) => IntValue(n); case FloatExp(x) => FloatValue(x) 
    }
    | all > StringValue
  )

  /** Parse a value input into a cell.  Called by Spreadsheet. */
  def parseUserValue(st: String): Cell = parseAll(userValue, st)
}

// =======================================================

object StatementParser{
  import ExpParser.{expr,cell,withExtent}

  /** Parser for a directive, <cell> = <expr>. */
  private def directive: Parser[Directive] = 
    (cell <~ lit("=")) ~ expr > toPair(Directive) 

  /** A parser for a value declaration, "val <name> = <expr>". */
  private def valDec: Parser[ValueDeclaration] =
    lit("val") ~> name ~ (lit("=") ~> expr) > toPair(ValueDeclaration)

  /** Parser for an expression in square brackets. */
  private def inSquare[A](p: Parser[A]): Parser[A] = 
    (lit("[") ~> p) <~ lit("]")

  /** A parser for a type. */
  private def typeP1: Parser[TypeT] = (
    lit("Int") > { _ => IntType }
    | lit("Float") > { _ => FloatType }
    | lit("Boolean") > { _ => BoolType }
    | lit("Row") > { _ => RowType }
    | lit("Column") > { _ => ColumnType }
    //     // TODO: and more
    | lit("List") ~> inSquare(typeP) > { t => ListType(t) }
    | upperName > { n => TypeParam(n) }
  )

  def typeP: Parser[TypeT] = 
    typeP1 ~ opt(lit("=>") ~> typeP) > { _ match {
      case (t,None) => t
      case (t1, Some(t2)) => FunctionType(List(), List(t1), t2)
        // IMPROVE: the "List()" looks odd.
        // FIXME: allow tuple of types in place of t1
    }}

  /** A parser for a list of parameters, "name1: type1, ..., namek: typek". */
  private def params: Parser[List[(String,TypeT)]] = {
    def param: Parser[(String,TypeT)] = (name <~ lit(":"))~typeP
    repSep(param, ",")
  }

  import FunctionType.TypeParameter

  private def typeConstraint: Parser[TypeParamConstraint] = (
    lit("<:") ~> (
      lit ("Eq") > (_ => EqTypeConstraint) 
      | lit("Num") > (_ => NumTypeConstraint) // MemberOf(TypeT.NumTypes))
    ) 
    | success(AnyTypeConstraint)
  )

  /** Parser for a single type parameter. */
  private  def typeParam: Parser[TypeParameter] =
    upperName ~ typeConstraint // > { t => (t, AnyTypeConstraint) } 

  /** Parser for type parameters. */
  private def typeParams: Parser[List[TypeParameter]] = 
    opt(inSquare(repSep(typeParam, ","))) > 
      { case Some(ts) => ts; case None => List() }

  /** A parser for a function declaration "def <name>(<params>) = <expr>". */
  def funDec: Parser[FunctionDeclaration] = 
    (lit("def") ~> name ~ typeParams ~ inParens(params)) ~ 
      ((lit(":") ~> typeP) ~ (lit("=") ~> expr)) >
    { case (((n,tps),ps), (rt,e)) => FunctionDeclaration(n, tps, ps, rt, e) }
 
  /** A parser for a declaration: either a value or function declaration. */
  def declaration = valDec | funDec

  /** A parser for a statement. */
  def statement: Parser[Statement] = withExtent(
    directive | declaration
  )

  /** A parser for a separator between statements, either a newline or a
    * semicolon.  Note: this consumes white space at the start of its input:
    * it should be sequenced with the preceding parser using `~~`. */
  def separator: Parser[String] = toLineEnd | consumeWhite ~> lit(";")

  /** A parser running `p` repeatedly. */
  def listOf[A <: Statement](p: Parser[A]): Parser[List[A]] = 
    p ~~ repeat1(separator ~> p) > toPair(_::_)

  /** Parse repeatedly with `p`, separated by `sep`, until `endMarker` is
    * reached. */
  def repeatUntil[A,B,C](p: Parser[A], sep: Parser[B], endMarker: Parser[C])
      : Parser[(List[A], C)] = (
    p ~~ (
      sep ~> repeatUntil(p, sep, endMarker) 
      |
      consumeWhite ~> endMarker > { end => (List[A](), end) }
    ) > { case (r1, (rs,end)) => (r1::rs, end) }
  )

  /** A parser for one or more statements. */
  def statements: Parser[List[Statement]] = 
    repeatUntil(statement, separator, atEnd) > (_._1)
    // listOf(statement)
  // statement ~~ repeat1(separator ~> statement) > toPair(_::_)

  /** Try to parse `input`, returning either the result or an error message. */
  def parseStatements(input: String): Either[List[Statement], String] = {
    val stmts = parseWith(statements, input)
    // stmts match{ 
    //   case Left(ss) => println(ss.mkString("\n"))
    //   case Right(msg) => //  println(msg)
    // }
    stmts
  }

}
