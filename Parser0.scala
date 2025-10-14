/* Base class for parsers, and parsers for Cells and CSV. */

package spreadsheet

import Parser._

/** Base class of various parsers, defining parsers for numeric values, and
  * for string literals. */
trait Parser0{
  /** A parser for an Int or Float. */
  protected def number: Parser[Exp] = {
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


  /** A parser for a separator between statements or branches in a cell match
    * expression: either a newline or a semicolon.  Note: this consumes white
    * space at the start of its input: it should be sequenced with the
    * preceding parser using `~~`. */
  def separator: Parser[String] = 
    //consumeWhiteNoNL ~> (lit("\n") | lit(";"))
    toLineEnd | consumeWhite ~> lit(";")

}



// =======================================================

/** A parser for a Cell. */
object CellParser extends Parser0{
  /** Parser for a value entered in a cell by the user. */
  private def userValue: Parser[Cell] = (
    (number <~ atEnd) > { 
      case IntExp(n) => IntValue(n); case FloatExp(x) => FloatValue(x) 
    }
    | lit("true") ~> atEnd ~> success(BoolValue(true))
    | lit("false") ~> atEnd ~> success(BoolValue(false))
      // Note: we allow trailing spaces above
    | (string <~~ atEnd) >  StringValue 
    | spot(_ != '\"') ~~ all > { case(c,st) => StringValue(c+:st) }
      // Note: don't consider escapes here.  TODO: think about this
    | all > ParseError 
      // Last case corresponds to unclosed quotes or illegal escapes.
  )

  /** Parse a value input into a cell.  Called by Spreadsheet. */
  def apply(st: String): Cell = parseAll(userValue, st)
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
    | lit("true") ~> success(BoolValue(true))
    | lit("false") ~> success(BoolValue(false))
    | lit("\"") ~~> stringField > StringValue
// FIXME: should probably allow strings without quotes
    | success(Empty())
  )

  /** A parser for a line of a CSV file. */
  def line: Parser[List[Cell]] = repSep(field, ",") <~ atEnd

  /** Parse `st`. */
  def apply(st: String): List[Cell] = parseAll(line, st)
}
