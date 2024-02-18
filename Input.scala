package spreadsheet

/** An input corresponding to `text` from `pos` onwards. */
class Input(private val text: Array[Char], private val pos: Int = 0){
  /** Auxilliary constructor. */
  def this(st: String) = this(st.toArray)

  private val len = text.length

  require(pos <= len,
    s"Advanced beyond end of ${text.mkString}: pos = $pos; len = $len")

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
  def advance(k: Int) = new Input(text, pos+k)

  /** An `Input` corresponding to dropping all white space from the start of
    * this. */
  def dropWhite = {
    def isWhite(c: Char) = c == ' ' || c == '\t' || c == '\n'
    var p = pos; while(p < len && isWhite(text(p))) p += 1
    if(p == pos) this else new Input(text, p)
  }

  /** Create an Extent corresponding to the prefix of this up until the start of
    * other. */
  def until(other: Input) = {
    assert(other.text eq text); new Extent(text, pos, other.pos)
  }

  override def toString = s"${text.drop(pos).mkString}"
}

// =======================================================

/** The source of an Exp or Value. */
trait Source{
  def asString: String

  def until(other: Source): Source
}

// =======================================================

/** The source of a parsed expression.  This represents the string
  * text[start..end). */
case class Extent(private val text: Array[Char], 
    private val start: Int, private val end: Int) extends Source{

  def asString = text.slice(start, end).mkString

  /** The extension of this until e. */
  def until(e: Extent): Extent = {
    assert(e.text eq text); new Extent(text, start, e.end)
  }

  /** The extension of this with other. */
  def until(other: Source) = CompoundSource(this, other)

  override def toString = s"Extent($asString)"

  override def equals(other: Any) = other match{
    case e: Extent => text == e.text && start == e.start && end == e.end
  }
}

// =======================================================

/** A source corresponding to cell(column, row). */
case class CellSource(column: Int, row: Int) extends Source{
  /** String name for column c. */
  private def colName(c: Int): String = {
    require(0 <= c && c < 26); (c+'A').toChar.toString
  }
  // Note: I'm not sure if this is the best place for this function. 

  def asString = { val cName = colName(column); s"#$cName$row" }

  def until(other: Source) = CompoundSource(this, other)
}

// =======================================================

/** A compound source. */
case class CompoundSource(s1: Source, s2: Source) extends Source{
  def asString = s1.asString + s2.asString

  def until(other: Source) = CompoundSource(this, other)
}
