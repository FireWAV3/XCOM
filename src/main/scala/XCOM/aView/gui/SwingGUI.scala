package XCOM.aView.gui
import XCOM.controller.{Controller, UpdateField}
import javax.swing.ImageIcon

import scala.swing._
import scala.swing.event.MouseClicked

class SwingGUI(c: Controller) extends Frame {
  listenTo(c)
  title = "Xcom"
  var menu = new BoxPanel(Orientation.Vertical){
    contents += new Label(){
      icon = new ImageIcon("/img/xcom_menu_icon.png")
    }
    contents += new Label(){
      text = "Welcome to XCOM!"
    }
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label(){
        text = "Please enter a Number between 1 and " + c.scenarioAmmount
      }
      contents += new TextField(){
      }
    }
    contents += new Label(){
      text = "Go!"
      listenTo(mouse.clicks)
      reactions +={
        case MouseClicked(scr,pt,mod,clicks,pops) => c.loadScenario(1)
      }
    }
  }

  val label = new Label(){
    text = "Welcome to XCOM!"
  }
  var bp = new BoxPanel(Orientation.Vertical){
    contents += label
    listenTo(mouse.clicks)
    reactions +={
      case MouseClicked(scr,pt,mod,clicks,pops) => c.info(Some("C1"))
    }
  }
  contents = bp
  contents = menu
  maximize()
  resizable = false
  open()
  reactions += {
    case event: UpdateField => label.text = c.output; repaint()
  }
}
