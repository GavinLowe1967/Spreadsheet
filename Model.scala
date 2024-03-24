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
    clearCells(); view.clearInfo()
    val fContents = scala.io.Source.fromFile(filename).mkString
    StatementParser.parseStatements(fContents) match{
      case Left(ss) => 
        TypeChecker(ss) match{
          case Ok(typeEnv) => statements = ss; update1()
          case FailureR(err) => view.addInfo(s"Type error: $err")
        }
// IMPROVE: store type env in first case. 

      case Right(msg) => 
        view.addInfo(s"Parse error: $msg"); println(s"Error!$msg")
    }
  }

  /** Clear calculated cells. */
  private def clearCells() = 
    for(c <- 0 until width; r <- 0 until height; if calculated(c)(r)){
      calculated(c)(r) = false; cells(c)(r) = Empty() 
    }

  /** Update cells based on statements.  Called by view. */
  def update() = {
    clearCells(); view.clearInfo(); update1()
  }

  /** Update cells based on statements.  Called internally. */
  private def update1() = {
    val env = new Environment(cells, calculated, height, width)
   
    // Iterate over statements, unless an error is found.
    //var ok = true; val iter = statements.iterator
    def handleError(err: ErrorValue) = {view.addInfo(err.msg)}
    //while(ok && iter.hasNext) ok = iter.next().perform(env, handleError)
    Statement.performAll(statements, env, handleError)
  }



}
