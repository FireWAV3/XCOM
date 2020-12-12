package XCOM.aView.gui
import XCOM.controller.{Controller, UpdateField}

import scala.swing._
import scala.swing.event.MouseClicked

class SwingGUI(c: Controller) extends Frame {
  listenTo(c)
  title = "Xcom"
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
  maximize()
  open()
  reactions += {
    case event: UpdateField => label.text = c.output; repaint()
  }
}
