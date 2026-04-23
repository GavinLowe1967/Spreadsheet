/* Parsers for Cells and CSV files. */

package spreadsheet

import Parser._

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
  def apply(st: String): Cell = parseAll(userValue, Input.fromField(st))
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
  def apply(st: String): List[Cell] = parseAll(line, Input.fromField(st))
}
