package spreadsheet

import scala.swing._
import java.awt.Font

/** The MainFrame of the application. */
class View(model: Model) extends MainFrame with ViewT{

  title = "Spreadsheet"

  // ========= Buttons and menus

  private val reloadButton = Button("Reload script"){
    model.reloadScript(); redisplay()
  }

  private val menuFont =  new Font(Font.SANS_SERIF, Font.PLAIN, 16) //MONOSPACED

  /** Make a MenuItem for Action, using font menuFont. */
  private def mkMenuItem(name: String)(effect: => Unit): MenuItem = {
    val item = new MenuItem(Action(name)(effect)); item.font = menuFont; item
  }

  /** Menu for operations. */
  private val operationsMenu = new Menu("Operations"){
    font = menuFont
  }

  /** Add an item labelled with name to the operations menu. */
  def addOperation(name: String) = {
    val mi = mkMenuItem(name){ model.executeOperation(name) }
    operationsMenu.contents += mi
  }

  menuBar = new MenuBar{
    contents += Swing.HStrut(5)
    contents += new Menu("File"){
      font = menuFont
      contents += mkMenuItem("Save sheet"){ model.saveSheet() }
      contents += new Separator
      contents += mkMenuItem("Exit"){ sys.exit() }
    }
    contents += operationsMenu
    contents += Swing.HGlue 
    contents += reloadButton
    contents += Swing.HStrut(5)
  }

  // private val saveButton = Button("Save"){
  //   model.saveSheet()
  // }

  // private val buttonPanel = new BoxPanel(Orientation.Horizontal){
  //   contents += reloadButton // ; contents += saveButton
  // }

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

  contents = new SplitPane(Orientation.Horizontal){ 
    border = Swing.EmptyBorder(10)
    // Panel containing spreadsheet and buttons
    topComponent = new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(5)
      contents += spreadsheet
      contents += Swing.VStrut(5)
      // contents += buttonPanel
      // contents += Swing.VStrut(5)
    }
    // Panel containing two text boxes.
    bottomComponent = new SplitPane(Orientation.Horizontal){ 
      topComponent = new ScrollPane(selectionBox)
      bottomComponent = new ScrollPane(infoBox)
      preferredSize = new Dimension(800,240); resizeWeight = 0.4 
    }
  }

  // contents = new BoxPanel(Orientation.Vertical) {
  //   border = Swing.EmptyBorder(10)
  //   // contents += new ScrollPane(selectionBox){    
  //   //   preferredSize = new Dimension(600,100) 
  //   // }
  //   // contents += spreadsheet
  //   // contents += Swing.VStrut(5)
  //   // contents += buttonPanel
  //   contents += splitPane
  //   // contents += mainPanel
  //   // contents += Swing.VStrut(5)
  //   // contents += new BoxPanel(Orientation.Horizontal){ 
  //   //   contents += textSplitPane 
  //   // }
  //   // contents += Swing.VStrut(5)
  //   // contents += new ScrollPane(infoBox){
  //   //   preferredSize = new Dimension(600,140) 
  //   // }
  // }

  // ========= Operations on the View =========

  def redisplay() = spreadsheet.repaint()

  /** Show `text` in the selection box. */
  def showSelection(text: String) = {
    selectionBox.text = text
    selectionBox.peer.select(0,0) // Move scrollbar to top of selectionBox
  }

  /** Add `text` to the information box. */
  def addInfo(text: String) = {
    infoBox.text += (text+"\n")
    infoBox.peer.select(0,0) // Move scrollbar to top of infoBox
  }

  def clearInfo() = infoBox.text = ""

  /** Clear the operations menu. */
  def clearOperations() = operationsMenu.contents.clear()
}
