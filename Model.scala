package spreadsheet

/** The model. */
class Model(val height: Int, val width: Int){
  /** The View, as seen from the Model. */ 
  private var view: ViewT = null

  /** Set the view to be `v`.  */
  def setView(v: ViewT) = view = v

  /** The cells holding the content.  Note: indexing is done by (column, row)
    * coordinates, following the spreadsheet convention. */
  val cells = Array.fill[Cell](width, height)(Empty())

  /** Record of which cells were calculated. */
  val calculated = Array.fill(width, height)(false)

  /** The statements defined in the script.  Set by loadFile. */
  private var statements = List[Statement]()

  /** The type environment resulting from checking the script.  Set by
    * loadFile. */
  private var typeEnv: TypeEnv = null

  private var scriptName: String = null

  private var sheetName: String = null

  /** Load statements from files `fname`.dir and fname.csv. */
  def loadScript(scN: String, shN: String) = {
    scriptName = scN; sheetName = shN
    if(sheetName != null) loadSheet()
// TODO: check sheetName exists
    reloadScript()
  }

  /** Remove comments from st. */
  private def removeComments(st: String): String = {
    var i = 0; val sb = new StringBuilder; val len = st.length
    while(i < len){
      if(st(i) == '/' && i+1 < len && st(i+1) == '/'){
        // advance to end of line
        i += 2; while(i < len && st(i) != '\n') i += 1
      }
      else{ sb += st(i); i += 1 }
    }
    // println(sb.toString)
    sb.toString
  }

  /** Reload script from the saved filename. */
  def reloadScript() = {
    clearCells(); view.clearInfo()
    val fContents = removeComments(scala.io.Source.fromFile(scriptName).mkString)
    // println(fContents+"END")
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
    val env = new Environment(
      cells, calculated, height, width, typeEnv.getEvaluationTypeEnv)
    // Iterate over statements, unless an error is found.
    def handleError(err: ErrorValue) = view.addInfo(err.msg)
    Execution.performAll(statements, env, handleError)
    view.redisplay()
  }

  /** Save the current sheet.  This writes just user input values. */
  def saveSheet() = {
// FIXME: check sheetName != null ... java.awt.FileDialog
    import java.io._
    val file = new File(sheetName)
    if (!file.exists()) file.createNewFile()
    val bw = new BufferedWriter(new FileWriter(file.getAbsoluteFile()))
    for(r <- 0 until height)  // Write row r
      bw.write(
        (0 until width).map(c =>
          if(!calculated(c)(r)) cells(c)(r).asCSV else ""
        ).mkString(",") + "\n"
      )
    bw.close()
  }

  /** Load the sheet from `sheetName`. */
  private def loadSheet() = {
    val file = new java.io.File(sheetName)
    if(file.exists){
      val lines = scala.io.Source.fromFile(file).getLines().toArray
      for(r <- 0 until lines.length){
        val fields = CSVParser(lines(r)).toArray 
        for(c <- 0 until fields.length) 
          cells(c)(r) = fields(c).withCellSource(c,r)
      }
    }
  }

}
