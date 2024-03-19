package spreadsheet

/** The View, as seen from the model or other components of the view. */
trait ViewT{
  /** Redisplay the view. */
  def redisplay(): Unit

  /** Show `text` in the selection box. */
  def showSelection(text: String): Unit

  /** Add `text` to the information box. */
  def addInfo(text: String): Unit

  /** Clear the information box. */
  def clearInfo(): Unit
}
