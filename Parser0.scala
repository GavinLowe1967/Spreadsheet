/* Base class for parsers, and parsers for Cells and CSV. */

package spreadsheet

import Parser._

/** Base class of various parsers, defining parsers specific to this project. */
trait Parser0{
  // ========= Numbers

  /** A parser for a non-negative Int or Float. */
  protected def posNum: Parser[Exp] = { 
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
    // Main parser
    digits ~~ opt(lit(".") ~~ digits) > {
      case (intPart, None) => IntExp(mkInt(intPart))
      case (intPart, Some((_,fracPart))) =>
        FloatExp(mkInt(intPart) + mkFloat(fracPart))
    }
  }

  /** A parser for an Int or Float. */
  protected def number: Parser[Exp] = (
    // Main parser: allow leading "-"
    lit("-") ~~ posNum > { 
      case (_,IntExp(n)) => IntExp(-n); case (_,FloatExp(x)) => FloatExp(-x) 
    } |
    posNum
  )

  // ========= Strings

  /** Parse an escaped character. */
  private def escapedChar: Parser[Char] = 
    spot(c => List('\\', '\"', '\'', 't', 'n', 'b', 'r', 'f').contains(c)
    ) > {
      case '\\' => '\\'; case '\"' => '\"'; case '\'' => '\''
      case 't' => '\t' ; case 'n' => '\n'; case 'b' => '\b';
      case 'r' => '\r'; case 'f' => '\f'
    }

  /** Parse a literal string, after the opening quotation mark. */ 
  private def string1: Parser[String] = (
    lit("\"") ~~> success("")
    | lit("\\") ~~> ( escapedChar ~~ string1 > { toPair(_+:_) } )
    | spot(c => c != '\\' && c != '\n') ~~ string1 > { toPair(_+:_) }
  )

  /** A parser for a string literal in the script. */
  protected def string: Parser[String] = lit("\"") ~~> string1 

  // ========= Wrapping parsers

  /** A parser for input consumed by p, surrounded by brackets, "{" and "}". */
  protected def inBrackets[A](p: Parser[A]): Parser[A] = 
    (lit("{") ~> p) <~ lit("}")

  /** Parser for an expression in square brackets. */
  protected def inSquare[A](p: Parser[A]): Parser[A] = 
    (lit("[") ~> p) <~ lit("]")

  // ========= Sequencing

  /** A parser for a separator between statements or branches in a cell match
    * expression: either a newline or a semicolon.  Note: this consumes white
    * space at the start of its input: it should be sequenced with the
    * preceding parser using `~~`. */
  protected def separator: Parser[String] = 
    //consumeWhiteNoNL ~> (lit("\n") | lit(";"))
    toLineEnd | consumeWhite ~> lit(";")

  /** A parser running `p` repeatedly, zero or more times, with separators in
    * between. */
  protected def listOf[A](p: Parser[A]): Parser[List[A]] = 
    p ~~ repeat1(separator ~> p) > toPair(_::_) | success(List()) 

  /** A parser running `p` repeatedly, one or more times, with separators in
    * between. */
  protected def listOfNonEmpty[A](p: Parser[A]): Parser[List[A]] = 
    p ~~ repeat1(separator ~> p) > toPair(_::_)
  //(p <~ separator) ~ listOf(p) > toPair(_::_)

  // ========= Adding Extent

  /** Lift `p`, so its result is annotated with its extent. */
  protected def withExtent[A <: HasExtent](p: => Parser[A]) = new Parser[A]{
    def apply(in: Input) = p(in) match{
      case s @ Success(exp, in1) => exp.setExtent(in.until(in1)); s
      case failure => failure
    }
  }   
}

// =======================================================

/** A parser for types. */
object TypeParser extends Parser0{
  /** A parser that consumes st and returns t. */
  private def mkP[T](st: String, t: T): Parser[T] = 
    keyword(st) ~~> success(t)

  /** A parser for a CellType. */
  def cellType: Parser[CellType] = (
    mkP("Int", IntType) | mkP("Float", FloatType)
    |  mkP("Boolean", BoolType) | mkP("String", StringType)
    // | keyword("Boolean")  > { _ => BoolType }
  )

  /** Parser for a tuple type or parenthesised type. */
  private def tupleOrParens: Parser[TypeT] =
    (lit("(") ~> typeP) ~ (
      lit(")") ~~> success(List[TypeT]()) // Note: don't consume white space
      | lit(",") ~> repSep(typeP, ",") <~ lit(")")
    ) > { case (e, es) => if(es.isEmpty) e else TupleType(e::es) }

  /** A parser for a type name, type parameter, tuple type, or parenthesised
    * type. */
  private def typeP1: Parser[TypeT] = (
    cellType
    | mkP("Unit", UnitType) | mkP("Row", RowType) | mkP("Column", ColumnType)
    | keyword("List") ~> inSquare(typeP) > { t => ListType(t) }
    | upperName > { n => TypeParam(n) }
    | tupleOrParens

  )

  /** A parser for a type. */
  def typeP: Parser[TypeT] = (
    typeP1 ~~ opt(consumeWhite ~~> lit("=>") ~> typeP) > { _ match {
      case (t,None) => t
      case (t1, Some(t2)) => FunctionType(List(), List(t1), t2)
        // IMPROVE: the "List()" looks odd.
    }}
//    | lit("(") ~> typeP <~ lit(")")
  )

  /** A parser for a type preceded by a colon. */
  def ofType: Parser[TypeT] = lit(":") ~> typeP
}

// =======================================================

/** A parser for a Cell. */
object CellParser extends Parser0{
  /** Parser for a value entered in a cell by the user. */
  private def userValue: Parser[Cell] = (
    (number <~ atEnd) > { 
      case IntExp(n) => IntValue(n); case FloatExp(x) => FloatValue(x) 
    }
    | keyword("true") ~> atEnd ~> success(BoolValue(true))
    | keyword("false") ~> atEnd ~> success(BoolValue(false))
      // Note: we allow trailing spaces above
    | (string <~~ atEnd) >  StringValue 
    | spot(_ != '\"') ~~ all > { case(c,st) => StringValue(c+:st) }
      // Note: don't consider escapes here.  TODO: think about this
    | all > ParseError 
      // Last case corresponds to unclosed quotes or illegal escapes.
  )

  /** Parse a value input into a cell.  Called by Spreadsheet. */
  def apply(st: String): Cell = parseAll(userValue, Input(st))
}

// =======================================================

/** A parser for a CSV file. */
object CSVParser extends Parser0{
  /** Parse a String field in a CSV file, after the opening quotation mark.  Two
    * quotation marks are replaced by a single one. */
  private def stringField: Parser[String] = (
    lit("\"\"") ~~> stringField > ("\""+_)
    | lit("\"") ~~> success("")
    | char ~~ stringField > toPair(_+:_)
  )

  /** A parser for a field in a CSV file. */
  private def field: Parser[Cell] = (
    number > { 
      case IntExp(n) => IntValue(n); case FloatExp(x) => FloatValue(x) 
    }
    | keyword("true") ~> success(BoolValue(true))
    | keyword("false") ~> success(BoolValue(false))
    | lit("\"") ~~> stringField > StringValue
// FIXME: should probably allow strings without quotes
    | success(Empty())
  )

  /** A parser for a line of a CSV file. */
  def line: Parser[List[Cell]] = repSep(field, ",") <~ atEnd

  /** Parse `st`. */
  def apply(st: String): List[Cell] = parseAll(line, Input(st))
}
