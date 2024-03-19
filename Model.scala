package spreadsheet

/** The model. */
class Model(val height: Int, val width: Int){
  /** The View, as seen from the Model. */ 
  private var view: ViewT = null

  /** Set the view to be `v`.  */
  def setView(v: ViewT) = view = v

  /** The cells holding the content.  Note: indexing is done by (row,
    * coordinate), following the spreadsheet convention. */
  val cells = Array.fill[Cell](width, height)(Empty())

  /** Record of which cells were calculated. */
  val calculated = Array.fill(width, height)(false)

  /** The statements defined in a file.  Set by loadFile. */
  private var statements = List[Statement]()

  private var filename: String = null

  /** Load statements from `fname`. */
  def loadFile(fname: String) = {
    filename = fname; reloadFile()
  }

  /** Reload statements from the saved filename. */
  def reloadFile() = {
    val fContents = scala.io.Source.fromFile(filename).mkString
    StatementParser.parseStatements(fContents) match{
      case Left(ss) => statements = ss; update()
      case Right(msg) => view.addInfo(s"Parse error: $msg"); println(s"Error!$msg")
    }
  }

  /** Update cells based on statements. */
  def update() = {
    // Clear calculated
    for(c <- 0 until width; r <- 0 until height) calculated(c)(r) = false
    val env = new Environment(cells, calculated, height, width)
    view.clearInfo()
    // Iterate over statements, unless an error is found.
    //var ok = true; val iter = statements.iterator
    def handleError(err: ErrorValue) = {view.addInfo(err.msg)}
    //while(ok && iter.hasNext) ok = iter.next().perform(env, handleError)
    Statement.performAll(statements, env, handleError)
  }



}
