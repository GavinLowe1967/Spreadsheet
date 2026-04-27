package spreadsheet

import scala.collection.mutable.HashMap

/** The model. */
class Model(val height: Int, val width: Int){
  /** The View, as seen from the Model. */ 
  private var view: ViewT = null

  /** Set the view to be `v`.  */
  def setView(v: ViewT) = {view = v; Execution.setView(v) }

  /** The environment in which the script is executed. */
  private val env = Environment(height, width, Model.initNameMap)

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
    reloadScript()
  }

  /** Parse the contents of a file (with comments removed). */
  private def parseContents(input: Input) = {
    StatementParser.parseStatements(input) match{
      case Left(ss) =>
        TypeChecker(ss) match{
          case Ok(te) => statements = ss; update1()
          case FailureR(err) => view.addInfo(s"Type error: $err")
        }

      case Right(msg) =>
        view.addInfo(s"Parse error: $msg") //; println(s"Error!$msg")
    }
  }

  /** Reload script from the saved filename. */
  def reloadScript() = {
    env.reset(); view.clearInfo()
    Input.fromFile(scriptName) match{
      case Left(input) => parseContents(input)
      case Right(err) => view.addInfo(err)
    }
  }

  /** Update cells based on statements.  Called by view. */
  def update() = { env.reset(); view.clearInfo(); update1() }

  /** Update cells based on statements.  Called internally. */
  private def update1() = {
    view.clearOperations()
    // Iterate over statements, unless an error is found.
    def handleError(err: ErrorValue) = view.addInfo(err.msg)
    Execution.performAll(statements, env, handleError)
    view.redisplay()
  }

  /** Perform the operation `name()`. */
  def executeOperation(name: String) = {
    def handleError(err: ErrorValue) = view.addInfo(err.msg)
    val call = CallStatement(FunctionApp(NameExp(name), List()))
    Execution.perform(env, handleError, call)
    view.redisplay()
  }

  /** Save the current sheet.  This writes just user input values. */
  def saveSheet() = {
// FIXME: check sheetName != null ... java.awt.FileDialog -- or FileChooser
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
      val source = scala.io.Source.fromFile(file)
      val lines = source.getLines().toArray
      source.close()
      for(r <- 0 until lines.length){
        val fields = CSVParser(lines(r)).toArray 
        for(c <- 0 until fields.length) 
          env.setUserCell(c, r, fields(c).withCSource(CellSource(c,r)))
      }
    }
    else println(s"File not found: $sheetName")
  }
}


object Model{
  /** Get the initial nameMap to use in an Environment. */ 
  def initNameMap = 
    new HashMap[String, Value] ++ (BuiltInFunctions.builtIns)
}
