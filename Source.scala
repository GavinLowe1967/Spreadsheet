package spreadsheet


/** The source of an Exp or Value. */
trait Source{
  /** The way this is displayed. */
  def asString: String
}

// =======================================================

/** The source of a parsed expression.  This represents the string
  * text[start..end). */
class Extent(
  private val text: Array[Char],
  private val start: Int, private val end: Int, val lineNumber: String)
    extends Source{

  def asString = text.slice(start, end).mkString

  /** The extension of this until e. */
  def until(e: Extent): Extent = {
    assert(e.text eq text); new Extent(text, start, e.end, lineNumber)
  }

  override def toString = s"Extent($asString)"

  override def equals(other: Any) = other match{
    case e: Extent => text == e.text && start == e.start && end == e.end
  }
}

// =======================================================

/** A source corresponding to cell(column, row). */
case class CellSource(column: Int, row: Int) extends Source{
  def asString = { val cName = CellSource.colName(column); s"#$cName$row" }
}

object CellSource{
  /** String name for column c. */
  def colName(c: Int): String = {
    def toChar(n: Int) = (n+'A').toChar
    if(c < 26) toChar(c).toString else List(toChar(c/26),toChar(c%26)).mkString
    //require(0 <= c && c < 26); (c+'A').toChar.toString
  }
}

// =======================================================
// Note: subclass CellWriteSource is in its own file.
// =======================================================

