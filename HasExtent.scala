package spreadsheet

/** The result of a parse with an Extent. */
trait HasExtent{
  /** The Extent representing the string from which this was produced. */
  protected var extent: Extent = null

  def getExtent = extent

  /** Set the Extent representing the string from which this was produced. */
  def setExtent(e: Extent) = extent = e  

  protected val tab = "" // Amount of space at start of lines.

  /** Lift an error value, by tagging on the extent of this. 
    * @param lineNum if true, include line number. */
  def liftError(error: ErrorValue, lineNum: Boolean = false) = {
    assert(extent != null, s"Null extent in $this")
    val lnString = if(lineNum) " at line "+extent.lineNumber else ""
    def extend(msg: String) = s"$msg$lnString\n${tab}in \"${extent.asString}\""
    error match{
      case TypeError(msg) => TypeError(extend(msg))
      case EvalError(msg) => EvalError(extend(msg))
    } 
  }

}
