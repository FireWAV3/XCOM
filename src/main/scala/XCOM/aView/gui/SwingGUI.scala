package XCOM.aView.gui
import XCOM.controller.controllerComponent._
import XCOM.util.UndoManager
import javax.swing.ImageIcon

import scala.swing.Swing.LineBorder
import scala.swing.event.MouseClicked
import scala.swing.{Label, _}
import scala.util.Try

class SwingGUI(c: ControllerInterface, uManager: UndoManager) extends Frame {
  listenTo(c)
  title = "Xcom Menu"


  var logo = new BoxPanel(Orientation.Horizontal){
    contents += new Label(){
      icon = new ImageIcon("src/main/scala/XCOM/aView/gui/img/xcom_menu_icon.png")
    }
  }

  val comboBox = new ComboBox(List("1","2")){
    maximumSize = new Dimension(30, 30)
  }


  var menu = new BoxPanel(Orientation.Vertical){
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label(){
        text = "Chose Level   "
      }

      contents += comboBox
    }

    contents += new Label(){
      text = "Go!"
      listenTo(mouse.clicks)
      reactions +={
        case MouseClicked(src,pt,mod,clicks,pops) => {
          c.loadScenario(comboBox.selection.index+1)
        }
      }
    }

    contents += new Label(){
      text = "Exit"
      listenTo(mouse.clicks)
      reactions +={
        case MouseClicked(scr,pt,mod,clicks,pops) => {
          Try(c.exit)
          System.exit(0)
        }
      }
    }
    val s = new Dimension(100, 100)
    minimumSize = s
    maximumSize = s
    preferredSize = s
  }

  val gridtext = new GridPanel(2,1){
    contents += new Label(){
      text = "Welcome to XCOM!"
    }
    contents += menu
  }

  contents = new GridPanel(2,1){
    contents += logo
    contents += gridtext
    border = LineBorder(java.awt.Color.BLACK, 2)
  }




  reactions += {
    case event: UpdateMenu => {
      var game = new GameField(c, uManager)
      game.main.visible = true
      invisible
      c.requestRepaint
    }
  }

  background = java.awt.Color.BLACK
  size = new Dimension(374,400)
  //resizable = false
  open()
  centerOnScreen()

  def invisible = visible = false

}


