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

  /** The statements defined in the script.  Set by loadFile. */
  private var statements = List[Statement]()

  /** The type environment resulting from checking the script.  Set by
    * loadFile. */
  private var typeEnv: TypeEnv = null

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
          case Ok(te) => statements = ss; typeEnv = te; update1()

          case FailureR(err) => view.addInfo(s"Type error: $err")
        } 

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
  def update() = { clearCells(); view.clearInfo(); update1() }

  /** Update cells based on statements.  Called internally. */
  private def update1() = {
    // println("New environment")
    val env = new Environment(
      cells, calculated, height, width, typeEnv)
    // Iterate over statements, unless an error is found.
    def handleError(err: ErrorValue) = {view.addInfo(err.msg)}
    Statement.performAll(statements, env, handleError)
    view.redisplay()
  }



}
