package XCOM.aView.gui
import XCOM.controller.{Controller, UpdateMenu}
import javax.swing.ImageIcon

import scala.swing.Swing.LineBorder
import scala.swing.event.MouseClicked
import scala.swing.{Label, _}
import scala.util.Try

class SwingGUI(c: Controller) extends Frame {
  listenTo(c)
  title = "Xcom Menu"


  var logo = new BoxPanel(Orientation.Horizontal){

    contents += new Label(){
      icon = new ImageIcon("/XCOM/aView/gui/img/xcom_menu_icon.png")
    }
    contents += new Label(){
      text = "XCOM"
    }
  }


  var menu = new BoxPanel(Orientation.Vertical){

    contents += new Label(){
      text = "Welcome to XCOM!"
    }

    contents += new BoxPanel(Orientation.Horizontal){
      background = java.awt.Color.GRAY
      contents += new Label(){
        text = "Chose Level   "
      }
      contents += new ComboBox(List("1","2")){
        background = java.awt.Color.GRAY
        maximumSize = new Dimension(30, 30)
      }
    }

    contents += new Label(){
      text = "Go!"
      listenTo(mouse.clicks)
      reactions +={
        case MouseClicked(src,pt,mod,clicks,pops) => {
          c.loadScenario(1)
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
    border = LineBorder(java.awt.Color.YELLOW, 2)
    background = java.awt.Color.GRAY
  }




  contents = new BorderPanel {
    add(logo,BorderPanel.Position.North)
    add(menu,BorderPanel.Position.Center)
    background = java.awt.Color.BLACK
  }

  reactions += {
    case event: UpdateMenu => {
      var game = new GameField(c)
      game.main.visible = true
      invisible
      c.requestRepaint
    }
  }

  background = java.awt.Color.BLACK
  size = new Dimension(700,700)
  //resizable = false
  open()

  def invisible = visible = false

}


