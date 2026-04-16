package spreadsheet
import scala.collection.mutable.ArrayBuffer
// import scala.io.Source

/** An input corresponding to `text` from `pos` onwards.
  * @param lineEnds an array giving the indices of ends of lines (including -1
  * and the overall length). 
  * @param lineNumbers an array giving line numbers in the associated file; the
  * line starting lineEnds(i)+1 is line lineNumbers(i) of the file.
  * @param filenames the corresponding filenames.
  */
class Input(
  private val text: Array[Char], private val pos: Int = 0,
  lineEnds: Array[Int], linenumbers: Array[Int], filenames: Array[String]
){
  private val len = text.length

  /* This represents text[pos..len). */

  def length = len-pos

  require(pos <= len,
    s"Advanced beyond end of ${text.mkString}: pos = $pos; len = $len")

  /** Is this empty, i.e. all the original text has been consumed. */
  def isEmpty = pos == len

  /** The first character of the text. */
  def head = text(pos)

  /** The character at position ix of this. */
  def apply(ix: Int) = text(pos+ix)

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
  def advance(k: Int) = new Input(text, pos+k, lineEnds, linenumbers, filenames)

  /** An `Input` corresponding to dropping all white space from the start of
    * this. */
  def dropWhite = {
    val p = Input.findNonWhite(text, pos)
    if(p == pos) this else new Input(text, p, lineEnds, linenumbers, filenames)
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

  /** Get the current line: a string giving the line number and optionally the
    * filename; the column number; and contents. */
  def getCurrentLine: (String, Int, String) = {
    // Find j s.t. le[0..j) <= pos < le[j..)
    // Inv le[0..i) <= pos < le[j..)
    require(pos <= lineEnds.last, s"$pos ${lineEnds.last}")
    var i = 0; var j = lineEnds.length
    while(i < j){
      val m = (i+j)/2 // i <= m < j
      if(lineEnds(m) <= pos) i = m+1 else j = m
    }
    assert(j > 0)
    val end = if(j == lineEnds.length) text.length else lineEnds(j)
    val thisLine = text.slice(lineEnds(j-1)+1, end).mkString
    val fname = filenames(j-1); val linenumber = linenumbers(j-1).toString
    val lineString = if(fname.isEmpty) linenumber else s"$linenumber of $fname"
    (lineString, pos-lineEnds(j-1)-1, thisLine)
  }
}

// ==================================================================

object Input{
  /** The position of the first non-white character from pos onwards. */
  private def findNonWhite(text: Array[Char], pos: Int): Int = {
    def isWhite(c: Char) = c == ' ' || c == '\t' || c == '\n'
    var p = pos; while(p < text.length && isWhite(text(p))) p += 1
    p
  }

  private def isWhite(c: Char) = c == ' ' || c == '\t'

  /** Get a filename, expected to be inclosed in quotation marks, starting from
    * position i0 in text.  If successful, return the filename and the index
    * of the next character (in a Left value).  Otherwise return Right value
    * explaining error. */
  private def getFilename(text: Array[Char], i0: Int)
      : Either[(String, Int), String] = {
    var i = i0; val len = text.length
    def mkError = {
      var ii = i0; while(ii < len && text(ii) != '\n') ii += 1
      val line = text.slice(i0-8, ii).mkString
      Right(s"Bad #include statement: $line")
    }
    // Find opening quote
    while(i < len && isWhite(text(i))) i += 1
    if(i == len || text(i) != '"') mkError
    else{
      i += 1; val start = i
      // Find closing quote
      while(i < len && text(i) != '"' && text(i) != '\n') i += 1
      if(i == len || text(i) != '"') mkError
      else{
        val filename = text.slice(start,i).mkString; i += 1
        // Check nothing else on line except white space.  
        while(i < len && isWhite(text(i))) i += 1
        if(i < len && text(i) != '\n') mkError
        else Left((filename, i)) // Don't consume newline
      }
    }
  }

  /** Preprocess text.  If successful, returns a tuple Left((processed,
    * lineEnds, lineNumbers, filenames)) where (1) processed is text with
    * comments removed and included files in-lined; (2) lineEnds is an array
    * of indices into processed giving the ends of lines; (3) each
    * lineNumbers(i) is the line number of the line starting at lineEnds(i)+1;
    * (4) filenames(i) is the corresponding filename.  Returns
    * Right(err) if there is an error.*/
  private def preprocess(text: Array[Char], filename: String)
      : Either[(Array[Char], Array[Int], Array[Int], Array[String]), String] = {
    var i = 0; val len = text.length; val ab = new ArrayBuffer[Char]
    var linenumber = 1
    val linenumbers = new ArrayBuffer[Int]; linenumbers += linenumber
    val le = new ArrayBuffer[Int]; le += -1 // for line ends
    val filenames = new ArrayBuffer[String]; filenames += filename
    // Record an end of line
    def newline() = {
      le += ab.length; linenumber += 1; 
      linenumbers += linenumber; filenames += filename
    }
    while(i < len){
      // Is the next character '/', not at the end of the file?
      val slash = text(i) == '/' && i+1 < len
      if(slash && text(i+1) == '/'){ // advance to end of line
        i += 2; while(i < len && text(i) != '\n') i += 1
      }
      else if(slash && text(i+1) == '*'){
        // Scan for corresponding "*/". `nesting` records the current level of
        // nesting of block comments.
        val start = linenumber; var nesting = 1; i += 2
        while(i < len && nesting > 0){
          if(text(i) == '*' && i+1 < len && text(i+1) == '/'){ 
            nesting -= 1; i += 2 
          }
          else if(text(i) == '/' && i+1 < len && text(i+1) == '*'){ 
            // nested comment
            nesting += 1; i += 2
          }
          else if(text(i) == '\n'){ // retain as a separator
            newline(); ab += text(i); i += 1
          }
          else i += 1
        }
        if(nesting > 0) 
          return Right(s"Parse error: unclosed block comment from line $start")
      } // end of "*" case
      else if(text.slice(i, i+8).mkString == "#include"){
        // Find filename
        getFilename(text,i+8) match{
          case Left((incFilename,j)) =>
            i = j; val included = scala.io.Source.fromFile(incFilename).toArray
            preprocess(included, incFilename) match{
              case Left((text1, lineEnds1, linenumbers1, filenames1)) =>
                assert(lineEnds1.length == linenumbers1.length &&
                  filenames1.length == linenumbers1.length)
                le ++= lineEnds1.map(_ + ab.length); ab ++= text1
                linenumbers ++= linenumbers1; filenames ++= filenames1
              case r @ Right(err) => return r
            } // end of inner match
          case Right(err) => return Right(err)
        }
      }
      else{ 
        if(text(i) == '\n') newline()
        ab += text(i); i += 1 
      }
    } // end of main while loop
    // Add artificial newline
    ab += '\n'; newline()
    Left((ab.toArray, le.toArray, linenumbers.toArray, filenames.toArray))
  }

  /** Try to construct an Input from st from file filename.  If unsuccessful,
    * return a Right value explaining the error. */
  private def apply0(st: String, filename: String): Either[Input, String] = {
    Failure.reset
    preprocess(st.toArray, filename) match{
      case Left((text, lineEnds, linenumbers, filenames)) =>
        val pos = findNonWhite(text, 0)
        Left(new Input(text, pos, lineEnds, linenumbers, filenames))
      case Right(err) => Right(err)
    }
  }

  /** Factory method.  This expects the script to have no unclosed comments of
    * incorrect #include statements. */
  def apply(st: String, filename: String = ""): Input = 
    apply0(st, filename) match{
      case Left(input) => input; case Right(err) => sys.error(err)
    }

  def fromFile(filename: String): Either[Input, String] =
    apply0(scala.io.Source.fromFile(filename).mkString, filename)

  /** Produce an Input corresponding to a field in a cell or a CSV file. */
  def fromField(st: String): Input = {
    Failure.reset; new Input(st.toArray, 0, null, null, null)
    // Note: the lineEnds, line numbers and filenames aren't used.
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

