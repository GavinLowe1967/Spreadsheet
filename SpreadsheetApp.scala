package spreadsheet

import scala.swing._

object SpreadsheetApp extends SimpleSwingApplication{

  private val model = new Model(100, 26)

  val view = new View(model)

  def top = view

  model.setView(view)


  override def main(args: Array[String]) = {
    val fname = args(0)
    model.loadFile(fname)
    view.redisplay()

    super.main(args) // Starts GUI

  }
}
