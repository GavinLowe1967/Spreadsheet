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

  /** The strings entered by users. */
  // private val strings = Array.fill(height, width)("")

  private val spreadsheetModel = model

  /** An editable text field. */
  class MyTextField(text: String) extends TextField(text){
    background = UserDataBackground; // peer.setOpaque = true
  }

  /** An uneditable text field containing `text`.   */
  class MyLabel(text: String, calc: Boolean) extends Label(text){
    background = 
      if(text.isEmpty) EmptyBackground 
      else if(calc) CalculatedBackground 
      else UserDataBackground
    peer.setOpaque(true)
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
      if(hasFocus) {
        val text =  // userData(column, row)
          cells(column)(row).asCell
          // if(calculated(column)(row)) cells(column)(row).asCell
          // else strings(column)(row)
        view.showSelection(text) // ****
        new MyTextField(text)
      }
      else /* if(calculated(column)(row)) */ {
        val v = cells(column)(row); assert(v != null)
        new MyLabel(v.asCell, calculated(column)(row))
      }
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
            // strings(column)(row) = v.toString
            //calculated(column)(row) = false
            val vString = v.toString
            cells(column)(row) = 
              if(vString.isEmpty) Empty() 
              else ExpParser.parseUserValue(vString)
            spreadsheetModel.update()
          }
          // println(s"($column, $row): ${userData(column,row)}, "+
          //   cells(column)(row))
        }
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
  val EmptyBackground = new Color(240,240,240)
  val UserDataBackground = new Color(220,255,255)
  val CalculatedBackground = new Color(220,255,220)
}
