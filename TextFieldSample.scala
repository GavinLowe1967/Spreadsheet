
import scala.swing.BorderPanel
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.TextField
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import BorderPanel.Position._

object TextFieldSample extends SimpleSwingApplication {

  /**
   * Main Window
   */
  def top = new MainFrame {
    title = "TextFieldSample"
    val textfield:TextField = new TextField("Input")
    //val label:Label = new Label("Output")

    contents = new BorderPanel (){
      add(textfield, North)
      //add(label, South)
    }

    // must specify 'keys' member of TextField, but not textfield it self.
    listenTo(textfield.keys)

    reactions += {
      case KeyPressed(_, Key.Enter, _, _) => println(textfield.text)
      case KeyPressed(_, key, _, _) => println(key)
    }
  }
}
