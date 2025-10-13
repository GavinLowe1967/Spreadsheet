package spreadsheet


/** An input corresponding to `text` from `pos` onwards.
  * @param lineEnds an array giving the indices of ends of lines (including -1
  * and the overall length). */
class Input(
    private val text: Array[Char], private val pos: Int = 0,
    private val lineEnds: Array[Int]
){
  /** Auxilliary constructor. */
  def this(st: String) = this(st.toArray, 0, Input.getLineEnds(st.toArray))

  private val len = text.length

  require(pos <= len,
    s"Advanced beyond end of ${text.mkString}: pos = $pos; len = $len")

  /** Indices of ends of lines. */
  // private val lineEnds: Array[Int] = {
  //   val les = new ArrayBuffer[Int]
  // }

  /** Is this empty, i.e. all the original text has been consumed. */
  def isEmpty = pos == len

  /** The first character of the text. */
  def head = text(pos)

  /** Does this start with `st`? */
  def startsWith(st: String): Boolean = {
    if(st.length > len-pos) false
    else{
      var i = 0; 
      while(i < st.length && st(i) == text(pos+i)) i += 1
      i == st.length
    }
  }

  /** An `Input` corresponding to advancing `k` places in this. */
  def advance(k: Int) = new Input(text, pos+k, lineEnds)

  /** An `Input` corresponding to dropping all white space from the start of
    * this. */
  def dropWhite = {
    def isWhite(c: Char) = c == ' ' || c == '\t' || c == '\n'
    var p = pos; while(p < len && isWhite(text(p))) p += 1
    if(p == pos) this else new Input(text, p, lineEnds)
  }

  /** Create an Extent corresponding to the prefix of this up until the start of
    * other. */
  def until(other: Input) = {
    assert(other.text eq text)
    new Extent(text, pos, other.pos, getCurrentLine._1)
  }

  def max(other: Input) = {
    assert(other.text eq text)
    if(pos >= other.pos) this else other
  }

  /** Is this more advanced than `other`? */
  def >= (other: Input) = {
    assert(other.text eq text); pos >= other.pos
  }

  override def toString = s"${text.drop(pos).mkString}"

  /** A truncated version of this: minimum of the rest of the line or 20
    * characters. */
  def trim: String = {
    var i = pos
    while(i < len && text(i) != '\n') i += 1
    text.drop(pos).take(i-pos max 20).mkString
  }

  /** Get the current line: line number, column number, and contents. */
  def getCurrentLine: (Int, Int, String) = {
    // Find j s.t. le[0..j) <= pos < le[j..)
    // Inv le[0..i) <= pos < le[j..)
    require(pos < lineEnds.last, s"$pos ${lineEnds.last}")
    var i = 0; var j = lineEnds.length
    while(i < j){
      val m = (i+j)/2 // i <= m < j
      if(lineEnds(m) <= pos) i = m+1 else j = m
    }
    assert(j > 0) 
    (j, pos-lineEnds(j-1)-1, text.slice(lineEnds(j-1)+1, lineEnds(j)).mkString)
  }
}

// ==================================================================

object Input{
  /** Get the indices of ends of lines in st. */
  def getLineEnds(st: Array[Char]): Array[Int] = {
    val le = new scala.collection.mutable.ArrayBuffer[Int]; le += -1
    var i = 0; val len = st.length
    while(i < len){
      if(st(i) == '\n') le += i 
      i += 1
    }
    // Note: we add a sentinel beyond the end of the input, to ensure all
    // positions in the file are strictly before the last line end.
    le += len+1; le.toArray
  }
}

// =======================================================

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
  private val start: Int, private val end: Int, val lineNumber: Int)
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

/** A source corresponding to a write to cell(column, row) by Directive dir. */
case class CellWriteSource(column: Int, row: Int, dir: Directive) extends Source{
  def asString = { 
    val cName = CellSource.colName(column); s"#$cName$row from $dir" 
  }
}
