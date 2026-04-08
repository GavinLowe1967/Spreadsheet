package spreadsheet

/** The model. */
class Model(val height: Int, val width: Int){
  /** The View, as seen from the Model. */ 
  private var view: ViewT = null

  /** Set the view to be `v`.  */
  def setView(v: ViewT) = view = v

  /** The environment in which the script is executed. */
  private val env = Environment(height, width)

  /** Get the environment. */
  def getEnv = env

  /** The statements defined in the script.  Set by loadFile. */
  private var statements = List[Statement]()

  private var scriptName: String = null // File name for the script

  private var sheetName: String = null // File name for the CSV file. 

  /** Load script from scN and CSV from shN. */
  def loadScript(scN: String, shN: String) = {
    scriptName = scN; sheetName = shN
    if(sheetName != null) loadSheet()
// TODO: check sheetName exists
    reloadScript()
  }

  /** Parse the contents of a file (with comments removed). */
  private def parseContents(fContents: String) = 
    StatementParser.parseStatements(fContents) match{
      case Left(ss) =>
        TypeChecker(ss) match{
          case Ok(te) => statements = ss; update1()
          case FailureR(err) => view.addInfo(s"Type error: $err")
        }

      case Right(msg) =>
        view.addInfo(s"Parse error: $msg") //; println(s"Error!$msg")
    }

  /** Reload script from the saved filename. */
  def reloadScript() = {
    env.reset(); view.clearInfo()
    val fContents = 
      Model.removeComments(scala.io.Source.fromFile(scriptName).mkString) 
    if(fContents != null) parseContents(fContents)
    else view.addInfo(s"Parse error: unclosed block comment")
  }

  /** Update cells based on statements.  Called by view. */
  def update() = { env.reset(); view.clearInfo(); update1() }

  /** Update cells based on statements.  Called internally. */
  private def update1() = {
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
        (0 until width).map(c => env.getUserCell(c,r).asCSV).mkString(",") + "\n"
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
          //env.setUserCell(c, r, fields(c).withCellSource(c,r))
          env.setUserCell(c, r, fields(c).withCSource(CellSource(c,r)))
      }
    }
  }

  // private val outer = this

  // object Testhooks{
  //   val removeComments = outer.removeComments _
  // }

}


object Model{

  /** Remove comments from st.  Return null if it contains an unclosed block
    * comment. */
  def removeComments(st: String): String = {
    var i = 0; val sb = new StringBuilder; val len = st.length
    while(i < len){
      // Is the next character '/', not at the end of the file?
      val slash = st(i) == '/' && i+1 < len
      if(slash && st(i+1) == '/'){ // advance to end of line
        i += 2; while(i < len && st(i) != '\n') i += 1
      }
      else if(slash && st(i+1) == '*'){
        // Scan for corresponding "*/". `nesting` records the current level of
        // nesting of block comments.
        var nesting = 1; i += 2
        while(i < len && nesting > 0){
          if(st(i) == '*' && i+1 < len && st(i+1) == '/'){ nesting -= 1; i += 2 }
          else if(st(i) == '/' && i+1 < len && st(i+1) == '*'){ // nested comment
            nesting += 1; i += 2
          }
          else if(st(i) == '\n'){ sb += st(i); i += 1 } // for line numbers
          else i += 1
        }
        if(nesting > 0) return null
      }
      else{ sb += st(i); i += 1 }
    }
    sb.toString
  }
}
