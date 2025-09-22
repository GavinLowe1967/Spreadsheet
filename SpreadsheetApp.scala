package spreadsheet

import scala.swing._

object SpreadsheetApp extends SimpleSwingApplication{

  private val model = new Model(100, 26)

  val view = new View(model)

  def top = view

  model.setView(view)


  override def main(args: Array[String]) = {
    // Expect to receive the scriptname as the first argument.  
    val scriptName = args(0); require(scriptName.endsWith(".dir"))
    // Optionally receive sheetname as second argument.
    val sheetName = if(args.length > 1) args(1) else null
    model.loadScript(scriptName, sheetName)
    view.redisplay()

    super.main(args) // Starts GUI

  }
}
