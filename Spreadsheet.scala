package spreadsheet

import scala.swing._
import event._
import java.awt.Color

/** The panel displaying the spreadsheet.
  * 
  * Based on code from "Programming in Scala".  */
class Spreadsheet(model: Model, view: ViewT) extends ScrollPane{
  import Spreadsheet._

  val height = model.height; val width = model.width 

  preferredSize = new Dimension(800,500)

  private val spreadsheetModel = model // Avoid aliasing by Table!

  private val env = model.getEnv

  /** An editable text field. */
  class MyTextField(text: String, background: Color) extends TextField(text){
    this.background = background 
  }

  /** An uneditable text field containing `text`.  */
  class MyLabel(text: String, colour: Color, background: Color)
      extends Label(text){
    this.background = background; foreground = colour; peer.setOpaque(true)
    xAlignment = Alignment.Right
  }

  /** The table displaying the cells. */
  private val table = new Table(height, width){
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new java.awt.Color(150, 150, 150)

    override def rendererComponent(
      isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int)
        : Component = {
      val cell = env.getCell(column,row); val text = cell.asCell
      val background = 
        if(text.isEmpty) EmptyBackground
        else if(env.isError(column,row)) ErrorBackground
        else if(env.isCalculated(column,row)){
          if(hasFocus) CalculatedWithFocusBackground else CalculatedBackground
        }
        else UserDataBackground
      if(hasFocus){
        val cell1 = env.getForSelection(column, row)
        view.showSelection(cell1.forSelection) 
        new MyTextField(text, background)
      }
      else{
        val colour = cell match{
          case _ : StringValue => StringTextColour; case _ => DefaultTextColour
        }
        new MyLabel(text, colour, background)
      }
    } // end of rendererComponent

    /** String to represent the entry in (row, column). */
    private def userData(column: Int, row: Int): String = {
      val v = this(row, column)
      if(v == null) "" else v.toString
    }

    reactions += {
      case TableUpdated(table, rows, column) =>
        for(row <- rows){
          val v = this(row, column)
          if(v != null){
            // Value entered in (row, column)
            val vString = v.toString
            val cell =
              if(vString.isEmpty) Empty() 
              else CellParser(vString).withCellSource(column,row)
            env.setUserCell(column, row, cell)
            spreadsheetModel.update()
          }
        }
      case e => println(e)
    }
  }

  /** The headers for the rows. */
  val rowHeader = new ListView((0 until height) map(_.toString)){
    fixedCellWidth = 30; fixedCellHeight = table.rowHeight
  }

  viewportView = table
  rowHeaderView = rowHeader
}

// =======================================================

object Spreadsheet{
  val EmptyBackground = new Color(250,250,250) // off-white
  val UserDataBackground = new Color(0.0F, 0.0F, 1.0F, 0.18F) // light blue
  val CalculatedBackground = new Color(0.0F, 1.0F, 0.0F, 0.18F) // light green
  val CalculatedWithFocusBackground = 
    new Color(0.0F, 1.0F, 0.0F, 0.4F) // darker green
  val ErrorBackground = new Color(1.0F, 0.0F, 0.0F, 0.18F) // light red
  val StringTextColour = new Color(100,100,100) // grey
  val DefaultTextColour = new Color(0,0,0) // black
}
