package XCOM.aView.gui

import XCOM.controller.controllerComponent._
import XCOM.model.PlayerStatus._
import XCOM.util.UndoManager
import javax.swing.{Icon, ImageIcon}

import scala.collection.mutable.ListBuffer
import scala.swing.Swing.{EmptyIcon, LineBorder}
import scala.swing.event.MouseClicked
import scala.swing.{BorderPanel, BoxPanel, Dimension, Frame, GridPanel, Label, MainFrame, Orientation}
import scala.util.Try
class GameField(c: ControllerInterface, uManager: UndoManager) extends Frame{




  def main = new MainFrame{
    title = "XCOM"
    listenTo(c)
    val localFile = System.getProperty("user.dir")

    var info = new GridPanel(1,2){

      minimumSize = new Dimension(1000, 150)
      preferredSize = new Dimension(1000, 150)
      var infoLabel =  new Label("Info"){
      }

     var next = new GridPanel(1,2){
        contents += new Label(){
            text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/next_button.png\" width = 100 height= 50> </html>"



            listenTo(mouse.clicks)
            reactions += {
              case MouseClicked(scr,pt,mod,clicks,pops) => uManager.doStep(c); c.next()
            }
        }
       contents += new Label(){
         text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/exit_button.png\" width = 100 height= 50> </html>"

         listenTo(mouse.clicks)
         reactions +={
           case MouseClicked(scr,pt,mod,clicks,pops) => {
             Try(c.exit)
             System.exit(0)
           }
         }
       }
      }

      contents += infoLabel
      contents += next
    }

    var chat = new BoxPanel(Orientation.Vertical){
      var chatREDLabel =  new Label("CHAT RED"){
        var messages = ListBuffer[String]()
        minimumSize = new Dimension(200, 380)
        preferredSize = new Dimension(200, 380)
      }
      var chatBLUELabel =  new Label("CHAT BLUE"){
        var messages = ListBuffer[String]()
        minimumSize = new Dimension(200, 380)
        preferredSize = new Dimension(200, 380)
      }
      contents += chatREDLabel
      contents += chatBLUELabel
    }

    var cInput = ""
    var highlightedCell = Array[IdLabel](new IdLabel(""), new IdLabel(""))


    var field = new GridPanel(c.field.sizeY+1,c.field.sizeX+1){
      var cells = Array.ofDim[IdLabel](c.field.sizeX+1,c.field.sizeY+1)
      for( y <- 1 to c.field.sizeY+1){
        for( x <- 1 to c.field.sizeX+1){
          val heroLabel = new IdLabel(""){
            recolor(this)
            listenTo(mouse.clicks)
            reactions +={
              case MouseClicked(scr,pt,mod,clicks,pops) => {
                mod match {
                  case 0 => {
                    if(id.contains("C")){
                      highlightedCell(0) = highlightedCell(1)
                      highlightedCell(1) = this
                      c.info(Some(id))
                    }
                  }
                  case 256 => { //right Mousebutton
                    val cell = highlightedCell(1)
                    val place = findCell(this)
                    uManager.doStep(c)
                    if(!c.move(cell.id,place._1+1,place._2+1)) uManager.undoClear(c)
                  }
                  case _ => { //middle Mousebutton
                    val cell = highlightedCell(1)
                    c.aim(Some(cell.id),Some(this.id))
                  }
                }
              }
            }
          }
          cells(x-1)(y-1) = heroLabel
          contents += heroLabel
        }
      }
    }

    def infoupdate(): Unit = {
      val output = "<html>" + c.output.replaceAll("\n","<br/>") + "</html>"
      info.infoLabel.text = output
      recolor(highlightedCell(0))
      highlightedCell(1).border = LineBorder(java.awt.Color.GREEN, 3)
      repaint()
    }

    def chatupdate(): Unit = {
      c.PlayerState match {
        case BLUE => {
          if(this.chat.chatBLUELabel.messages.size >= 5){
            this.chat.chatBLUELabel.messages = rotateArray(this.chat.chatBLUELabel.messages,c.output)
          } else {
            this.chat.chatBLUELabel.messages += c.output
          }
          var chatMessage = "CHAT BLUE\n\n"
          for (x <- this.chat.chatBLUELabel.messages) {
            chatMessage = chatMessage + x
            chatMessage += "\n"
          }
          this.chat.chatBLUELabel.text = "<html>" + chatMessage.replaceAll("\n","<br/>") + "</html>"
          repaint()
        }
        case RED => {
          if(this.chat.chatREDLabel.messages.size >= 5){
            this.chat.chatREDLabel.messages = rotateArray(this.chat.chatREDLabel.messages,c.output)
          } else {
            this.chat.chatREDLabel.messages += c.output
          }
          var chatMessage = "CHAT RED\n\n"
          for (x <- this.chat.chatREDLabel.messages) {
            chatMessage += x
            chatMessage += "\n"
          }
          this.chat.chatREDLabel.text = "<html>" + chatMessage.replaceAll("\n","<br/>") + "</html>"
          repaint()
        }
      }
    }

    def shootupdate() = {
        new DecisionPanel(c,c.output, uManager)
    }

    def fieldupdate() = {
      for(a <- 0 to field.cells.length-1){
        for (b <- 0 to field.cells(a).length-1){
          field.cells(a)(b).id = findType(a,b)
          recolor(field.cells(a)(b))
        }
      }
      highlightedCell(0) = new IdLabel("")
      highlightedCell(1) = new IdLabel("")
      info.infoLabel.text = ""
      repaint()
      chatupdate()
    }

    def rotateArray(array: ListBuffer[String], string: String): ListBuffer[String] = {
      var newArray = ListBuffer[String]()
      for (x <- 1 to array.size-1){
        newArray += array(x)
      }
      newArray += string
      newArray
    }

    def findCell(cell: IdLabel): (Int, Int) = {
      for (x <- 0 to field.cells.size){
        val y = field.cells(x).indexOf(cell)
        if (y != -1) return (x,y)
      }
      (-1,-1)
    }

    def findType(x: Int, y: Int): String ={
      for (a <- c.getCharacters) {
        if (a._2 == x && a._3 == y) {
          return a._1
        }
      }
      for (a <- c.getRocks) {
        if (a._1 == x && a._2 == y) {
          return "R"
        }
      }
      "X"
    }

    def recolor(cell: IdLabel): Unit ={
      cell.id match {
        case "X" => {
          cell.icon = EmptyIcon
          cell.text = ""
          cell.background = java.awt.Color.BLACK
          cell.border = LineBorder(java.awt.Color.BLACK, 1)
        }
        case "R" => {
          cell.icon = new ImageIcon("src/main/scala/XCOM/aView/gui/img/rock.png")
          //cell.text = cell.id
          cell.background = java.awt.Color.GRAY
          cell.border = LineBorder(java.awt.Color.GRAY, 3)
        }
        case _ => {

          //cell.text = cell.id
         c.getCharactersSide(cell.id) match {
           case 0 => {
             cell.border = LineBorder(java.awt.Color.BLUE, 3)
             //cell.icon = new ImageIcon("src/main/scala/XCOM/aView/gui/img/rifle_blue.png")
             cell.text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/sniper_blue.png\" width = "+cell.size.height+" height= "+cell.size.height +"> </html>"
           }
           case 1 => {
             cell.border = LineBorder(java.awt.Color.RED, 3)
             cell.text = "<html> <img src=\"file:///"+ localFile +"/src/main/scala/XCOM/aView/gui/img/tank_red.png\" width = "+cell.size.height+" height= "+cell.size.height +"> </html>"
           }
           case _ => cell.border = LineBorder(java.awt.Color.YELLOW, 3)
         }
        }
      }
    }

    reactions += {
      case event: UpdateInfo => infoupdate()
      case event: UpdateText => chatupdate()
      case event: UpdateField => fieldupdate()
      case event: UpdateShoot => shootupdate()
    }

    contents = new BorderPanel{
      add(field,BorderPanel.Position.Center)
      add(info,BorderPanel.Position.South)
      add(chat,BorderPanel.Position.East)
    }

    size = new Dimension(1500,800)
    maximize()
    resizable = false

  }
}

class IdLabel(var id: String, string0: String, icon0: Icon) extends Label{
  text = string0
  icon = icon0
  def this(id: String){
    this(id,id,EmptyIcon)
  }

  override def toString(): String = "id: " + id + "\ttext: " + text
}


class WinFrame(c:ControllerInterface) extends MainFrame {
  listenTo(c)
  title = "Congratulation"

  contents = new Label(){
    text = c.output
  }

  size = new Dimension(400,150)
  resizable = false
  visible = true
  centerOnScreen()
}

class DecisionPanel(c:ControllerInterface,output:String, uManager: UndoManager) extends MainFrame {
    listenTo(c)
    title = "Affirmation needed"
    val question = new GridPanel(1,2){
      var yes = new Label("Yes"){
        listenTo(mouse.clicks)
        reactions += {
          case MouseClicked(scr,pt,mod,clicks,pops) =>
            uManager.doStep(c)
            Try(c.shoot(true))

        }
      }
      var no = new Label("No"){
        listenTo(mouse.clicks)
        reactions += {
          case MouseClicked(scr,pt,mod,clicks,pops) =>  c.shoot(false)
            dispose()
        }
      }
      contents += yes
      contents += no
    }

    contents = new GridPanel(2,1){
      contents += new Label(output)
      contents += question
    }
    size = new Dimension(400,150)
    resizable = false
    visible = true
    centerOnScreen()

  reactions += {
    case event: UpdateField => dispose()
    case event: UpdateText => dispose()
  }

}