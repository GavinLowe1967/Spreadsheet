package spreadsheet

import scala.swing._
import event._
import java.awt.Color

/** The panel displaying the spreadsheet.
  * 
  * Based on code from "Programming in Scala".  */
class Spreadsheet(model: Model, view: ViewT) extends ScrollPane{
  import Spreadsheet._

  val height = model.height
  val width = model.width

  /** The cells, holding Values. */
  private val cells = model.cells

  /** Indication of which cells were calculated by directives. */
  private val calculated = model.calculated

  private val spreadsheetModel = model // Avoid aliasing by Table!

  /** An editable text field. */
  class MyTextField(text: String) extends TextField(text){
    background = UserDataBackground; // peer.setOpaque = true
  }

  /** An uneditable text field containing `text`.  
    * @param calc was the value in this field calculated by a directive? */
  class MyLabel(text: String, calc: Boolean, colour: Color, hasFocus: Boolean)
      extends Label(text){
    // editable = false
    background = 
      if(text.isEmpty) EmptyBackground 
      else if(calc){
        if(hasFocus) CalculatedWithFocusBackground else CalculatedBackground
      }
      else UserDataBackground
    foreground = colour; peer.setOpaque(true)
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
      val cell = cells(column)(row); val text = cell.asCell
      val calc = calculated(column)(row)
      val colour = cell match{ 
        case _ : StringValue => StringTextColour; case _ => DefaultTextColour
      }
      if(hasFocus) view.showSelection(text)
      if(hasFocus && !calc) new MyTextField(text)
      else new MyLabel(text, calc, colour, hasFocus)

      // if(hasFocus) {
      //   val text = cells(column)(row).asCell
      //   view.showSelection(text) // ****
      //   if(calculated(column)(row)) new MyLabel(text, true, true)
      //   else new MyTextField(text)
      // }
      // else /* if(calculated(column)(row)) */ {
      //   val v = cells(column)(row); assert(v != null)
      //   new MyLabel(v.asCell, calculated(column)(row), hasFocus)
      // }
      // else{
      //   val st = strings(column)(row); assert(st != null)
      //   new MyLabel(st, false)
      // }
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
            val vString = v.toString
            cells(column)(row) = 
              if(vString.isEmpty) Empty() 
              else CellParser(vString)
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
  val StringTextColour = new Color(100,100,100) // grey
  val DefaultTextColour = new Color(0,0,0) // black
}
