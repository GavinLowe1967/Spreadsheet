package spreadsheet 

import scala.swing._
import event._
import java.awt.{Color,Font}
import java.awt.event.{MouseEvent,MouseAdapter}

/** The panel displaying the spreadsheet.
  * 
  * Based on code from "Programming in Scala".  */
class Spreadsheet(model: Model, view: ViewT) extends ScrollPane{
  import Spreadsheet._

  private val env = model.getEnv
  // private val height = env.height; private val width = env.width 

  preferredSize = new Dimension(800,550)

  private val spreadsheetModel = model // Avoid aliasing by Table!

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

  /** Make the table displaying the cells. */
  private def mkTable() = new Table(env.height, env.width){
    rowHeight = Spreadsheet.rowHeight
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true; gridColor = new java.awt.Color(150, 150, 150)

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
        val err = cell.forError
        val forSelection = cell.source match{
          case CellWriteSource(_,_,d) =>
            val e = d.getExtent
            err+s"\nFrom cell write at line ${e.lineNumber}:\n"+e.asString
          case _ => err
        }
        view.showSelection(forSelection) 
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
      case TableUpdated(tab, rows, column) =>
        for(row <- rows){
          val v = this(row, column)
          if(v != null){
            // Value entered in (row, column)
            val vString = v.toString
            val cell =
              if(vString.isEmpty) Empty() 
              else CellParser(vString).withCSource(CellSource(column,row))
            env.setUserCell(column, row, cell)
            spreadsheetModel.update()
          }
        }
      case e => println(e)
    }
    
    /** Create a pop-up menu when the header for column `index` is pressed. */
    private def mkPopupMenu(index: Int) = new PopupMenu{
      import Spreadsheet.mkMenuItem
      contents += mkMenuItem("Insert column left"){ addColumn(index) }
      contents += mkMenuItem("Insert column right"){ addColumn(index+1) }
    }

    private val table = this
    private val tableHeader = peer.getTableHeader

    /** Mouse listener for the top header. */
    private val mouseListener = new MouseAdapter{
      override def mouseClicked(e: MouseEvent): Unit = {
        val index = tableHeader.getColumnModel.getColumnIndexAtX(e.getX)
        val menu = mkPopupMenu(index); menu.show(table, e.getX, e.getY)
      }
    }

    tableHeader.addMouseListener(mouseListener)
  } // end of mkTable

  viewportView = mkTable()

  // =========

  /** Add a new row with index `index`. */ 
  private def addRow(index: Int): Unit = {
    // Update env; add row to table and rowHeader.
    env.addRow(index); viewportView = mkTable(); rowHeaderView = mkRowHeader()
    // Re-run script.
    spreadsheetModel.update()
  }

  /** Add a new column with index `index`. */
  private def addColumn(index: Int): Unit = {
    // Update env; add column to table 
    env.addColumn(index); viewportView = mkTable()
    // Re-run script.
    spreadsheetModel.update()
  }

  /** The headers for the rows. */
  private def mkRowHeader() = new ListView((0 until env.height) map(_.toString)){
    fixedCellWidth = 30; fixedCellHeight = Spreadsheet.rowHeight

    /** Create a popup menu, when the row header for `index` is pressed. */
    def mkPopupMenu(index: Int) = new PopupMenu{
      import Spreadsheet.mkMenuItem
      contents += mkMenuItem("Insert row above"){ addRow(index) }
      contents += mkMenuItem("Insert row below"){ addRow(index+1) }
    }

    listenTo(mouse.clicks, mouse.moves)

    reactions += {
      case m: MousePressed =>
        val p = m.point; val index = peer.locationToIndex(p)
        val menu = mkPopupMenu(index); menu.show(this, p.x, p.y)
    }
  } // end of mkRowHeader

  rowHeaderView = mkRowHeader()

  //println(s"columnHeaderView = "+columnHeaderView)
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
  val DefaultTextColour = new Color(0,0,0) // 
  
  /** Font to use in menus. */
  val menuFont =  new Font(Font.SANS_SERIF, Font.PLAIN, 16)

  /** Make a MenuItem for Action, using font menuFont. */
  def mkMenuItem(name: String)(effect: => Unit): MenuItem = {
    val item = new MenuItem(Action(name)(effect))
    item.font = Spreadsheet.menuFont; item
  }

  /** Height of each row. */
  val rowHeight = 25
}
