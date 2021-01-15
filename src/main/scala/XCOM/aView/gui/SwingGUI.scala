package XCOM.aView.gui
import XCOM.controller.controllerComponent._
import XCOM.util.UndoManager

import scala.collection.mutable.ListBuffer
import scala.swing.Swing.LineBorder
import scala.swing.event.MouseClicked
import scala.swing.{Label, _}
import scala.util.Try

class SwingGUI(c: ControllerInterface, uManager: UndoManager) extends Frame {
  listenTo(c)
  title = "Xcom Menu"
  val localFile = System.getProperty("user.dir")


  var logo = new BoxPanel(Orientation.Horizontal){
    background = java.awt.Color.BLUE.darker().darker().darker()
    contents += new Label(){
      background = java.awt.Color.BLUE.darker().darker().darker()
      text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/xcom_menu_icon.png\" width = 400 height= 200> </html>"
    }
  }

  var selectLevelList = new ListBuffer[String]()
  for (x <- 1 to  c.scenarioAmmount ){selectLevelList.append(x.toString)}
  val comboBox = new ComboBox(selectLevelList){
    maximumSize = new Dimension(28, 28)
    background = java.awt.Color.BLUE.darker().darker().darker()
  }


  var menu = new GridPanel(3,1){

    background = java.awt.Color.BLUE.darker().darker().darker()

    contents += new GridPanel(1,3){
      background = java.awt.Color.BLUE.darker().darker().darker()

      contents += new Label(){
        background = java.awt.Color.BLUE.darker().darker().darker()
        text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/level.png\" width = 200 height= 50> </html>"
      }

      contents += new GridPanel(1,2){
        background = java.awt.Color.BLUE.darker().darker().darker()
        contents += comboBox
        contents += new Label()
      }

    }

    contents += new Label(){
      background = java.awt.Color.BLUE.darker().darker().darker()
     // text = "Go!"
      //icon = new ImageIcon("src/main/scala/XCOM/aView/gui/img/start_button.png")
      text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/start_button.png\" width = 100 height= 50> </html>"

      listenTo(mouse.clicks)
      reactions +={
        case MouseClicked(src,pt,mod,clicks,pops) => {
          c.loadScenario(comboBox.selection.index+1)
        }
      }
    }

    contents += new Label(){
      background = java.awt.Color.BLUE.darker().darker().darker()
      //text = "Exit"
      //icon = new ImageIcon("src/main/scala/XCOM/aView/gui/img/exit_button.png")
      text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/exit_button.png\" width = 100 height= 50> </html>"
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
    background = java.awt.Color.BLUE.darker().darker().darker()
    contents += new Label(){
     // text = "Welcome to XCOM!"
     // icon = new ImageIcon("src/main/scala/XCOM/aView/gui/img/welcome_to_xcom.png")
      text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/welcome_to_xcom.png\" width = 300 height= 100> </html>"
    }
    contents += menu
  }

  contents = new GridPanel(2,1){
    background = java.awt.Color.BLUE.darker().darker().darker()
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

  background = java.awt.Color.BLUE.darker().darker().darker()
  size = new Dimension(500,700)
  //resizable = false
  open()
  centerOnScreen()
  def invisible = visible = false

}


