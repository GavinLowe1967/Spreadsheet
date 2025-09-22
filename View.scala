package spreadsheet

import scala.swing._
import java.awt.Font

// import scala.swing.event.KeyPressed
// import scala.swing.event.Key

/** The MainFrame of the application. */
class View(model: Model) extends MainFrame with ViewT{

  title = "Spreadsheet"

  // ========= Buttons

  // private val updateButton = Button("Update"){ 
  //   model.update(); redisplay()
  // }
// TODO: do we need the above?

  private val reloadButton = Button("Reload"){
    model.reloadScript(); redisplay()
    // spreadsheet.showSelectedText();
  }

  private val saveButton = Button("Save"){
    model.saveSheet()
  }

  private val buttonPanel = new BoxPanel(Orientation.Horizontal){
    // contents += updateButton
    contents += reloadButton
    contents += saveButton
  }

  // ========= Text boxes

  /** The font to be used in the boxes.  Needs to be fixed-with to get correct
    * alignment in parse errors. */
  private val theFont = new Font(Font.MONOSPACED, Font.PLAIN, 16) 
  //new Font("SansSerif", Font.PLAIN, 16)

  /** A box giving the value of the currently selected cell. */
  private val selectionBox = new TextArea{
    editable = false; font = theFont
    wordWrap = true; lineWrap = true
    rows = 10
  }  

  /** A box giving information, e.g. errors. */
  private val infoBox = new TextArea{
    editable = false; font = theFont
    rows = 10
  }  
 
  // ========= Set up the frame

  private val spreadsheet = new Spreadsheet(model, this)

  // val textfield:TextField = new TextField("Input")

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
    // contents += selectionBox
    // contents += Swing.VStrut(5)
    // contents += infoBox
    // contents += Swing.VStrut(5)
    // contents += textfield
  }

  // listenTo(textfield.keys)

  // reactions += {
  //   case KeyPressed(_, Key.Enter, _, _) => println(textfield.text)
  //   case KeyPressed(_, key, _, _) => println(key)
  // }

  def redisplay() = spreadsheet.repaint()

  /** Show `text` in the selection box. */
  def showSelection(text: String) = selectionBox.text = text 

  /** Add `text` to the information box. */
  def addInfo(text: String) = {
    // println(s"<<$text>>")
    infoBox.text += (text+"\n")
    infoBox.peer.select(0,0) // Move scrollbar to top of infoBox
  }

  def clearInfo() = infoBox.text = ""
}
