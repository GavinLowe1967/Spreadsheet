package spreadsheet

import scala.swing._
import java.awt.Font

/** The MainFrame of the application. */
class View(model: Model) extends MainFrame with ViewT{

  title = "Spreadsheet"

  // ========= Buttons

  private val reloadButton = Button("Reload"){
    model.reloadScript(); redisplay()
  }

  private val saveButton = Button("Save"){
    model.saveSheet()
  }

  private val buttonPanel = new BoxPanel(Orientation.Horizontal){
    contents += reloadButton; contents += saveButton
  }

  // ========= Text boxes

  /** The font to be used in the boxes.  Needs to be fixed-with to get correct
    * alignment in parse errors. */
  private val theFont = new Font(Font.MONOSPACED, Font.PLAIN, 16) 

  /** A box giving the value of the currently selected cell. */
  private val selectionBox = new TextArea{
    editable = false; font = theFont; wordWrap = true; lineWrap = true
    rows = 10
  }  

  /** A box giving information, e.g. errors. */
  private val infoBox = new TextArea{
    editable = false; font = theFont; rows = 10
  }  
 
  // ========= Set up the frame

  private val spreadsheet = new Spreadsheet(model, this)

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10)
    contents += new ScrollPane(selectionBox){    
      preferredSize = new Dimension(600,100) 
    }
    contents += spreadsheet
    contents += Swing.VStrut(5)
    contents += buttonPanel
    contents += Swing.VStrut(5)
    contents += new ScrollPane(infoBox){
      preferredSize = new Dimension(600,140) 
    }
  }

  def redisplay() = spreadsheet.repaint()

  /** Show `text` in the selection box. */
  def showSelection(text: String) = selectionBox.text = text 

  /** Add `text` to the information box. */
  def addInfo(text: String) = {
    infoBox.text += (text+"\n")
    infoBox.peer.select(0,0) // Move scrollbar to top of infoBox
  }

  def clearInfo() = infoBox.text = ""
}
