package spreadsheet

import scala.swing._
import java.awt.Font

// import scala.swing.event.KeyPressed
// import scala.swing.event.Key

/** The MainFrame of the application. */
class View(model: Model) extends MainFrame with ViewT{

  title = "Spreadsheet"

  // ========= Buttons

  private val updateButton = Button("Update"){ 
    model.update(); redisplay()
  }

  private val buttonPanel = new BoxPanel(Orientation.Horizontal){
    contents += updateButton
  }

  // ========= Text boxes

  private val theFont = new Font("SansSerif", Font.PLAIN, 16)

  private val selectionBox = new TextArea{
    editable = false; font = theFont
    wordWrap = true; lineWrap = true
    rows = 10
    // maximumSize =  new Dimension(600, 80)
  }  

/*
  private val infoBox = new TextArea{
    editable = false; font = theFont
    maximumSize =  new Dimension(600, 80)
    rows = 4
  }  
 */

  // ========= Set up the frame

  private val spreadsheet = new Spreadsheet(model, this)

  // val textfield:TextField = new TextField("Input")

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10)
    contents += spreadsheet
    contents += Swing.VStrut(5)
    contents += buttonPanel
    contents += Swing.VStrut(5)
    // contents += selectionBox
    contents += new ScrollPane(selectionBox){    
      preferredSize = new Dimension(600,120) 
    }
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

  def showSelection(text: String) = selectionBox.text = text 

}
