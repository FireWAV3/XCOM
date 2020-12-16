package XCOM.aView.gui

import XCOM.controller.{Controller, UpdateInfo, UpdateText}
import XCOM.model.PlayerStatus._

import scala.collection.mutable.ListBuffer
import scala.swing.Swing.LineBorder
import scala.swing.event.MouseClicked
import scala.swing.{BorderPanel, BoxPanel, Dimension, Frame, GridPanel, Label, MainFrame, Orientation}
import scala.util.{Failure, Success, Try}

class GameField(c: Controller) extends Frame{


  def main = new MainFrame{
    title = "XCOM"
    listenTo(c)

    var info = new BoxPanel(Orientation.Horizontal){
      var infoLabel =  new Label("Info"){
        minimumSize = new Dimension(1000, 150)
        preferredSize = new Dimension(1000, 150)
      }

      contents += infoLabel
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
    var highlightedCell = Array[Label](new Label(), new Label())


    var field = new GridPanel(c.field.sizeY+1,c.field.sizeX+1){
      var cells = Array.ofDim[Label](c.field.sizeX+1,c.field.sizeY+1)
      for( y <- 1 to c.field.sizeY+1){
        for( x <- 1 to c.field.sizeX+1){
          Try(c.testRock(x,y)) match {
            case Success(value) =>{
              Try(c.getHero(x,y)) match {
                case Success(value) =>{
                  val heroLabel = new Label(value.displayname){
                      background = java.awt.Color.YELLOW
                      border = LineBorder(java.awt.Color.YELLOW, 3)
                      foreground = java.awt.Color.BLACK
                      listenTo(mouse.clicks)
                      reactions +={
                        case MouseClicked(scr,pt,mod,clicks,pops) => {
                          highlightedCell(0) = highlightedCell(1)
                          highlightedCell(1) = this
                          c.info(Some(text))}
                      }
                  }
                  cells(x-1)(y-1) = heroLabel
                  contents += heroLabel
                }
                case Failure(exception) =>{
                  val emptyLabel = new Label(){
                    val id = "X"
                    background = java.awt.Color.BLACK
                    border = LineBorder(java.awt.Color.BLACK, 1)
                  }
                  cells(x-1)(y-1) = emptyLabel
                  contents += emptyLabel
                }
              }
            }
            case Failure(exception) =>{
              val rockLabel = new Label("R"){
                val id = "R"
                background = java.awt.Color.GRAY
                border = LineBorder(java.awt.Color.GRAY, 3)
                foreground = java.awt.Color.BLACK
              }
              cells(x-1)(y-1) = rockLabel
              contents += rockLabel
            }
          }

        }
      }




    }


    def infoupdate(): Unit = {
      val output = "<html>" + c.output.replaceAll("\n","<br/>") + "</html>"
      info.infoLabel.text = output
      highlightedCell(0).border = LineBorder(java.awt.Color.YELLOW, 3)
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

    def rotateArray(array: ListBuffer[String], string: String): ListBuffer[String] = {
      var newArray = ListBuffer[String]()
      for (x <- 1 to array.size-1){
        newArray += array(x)
      }
      newArray += string
      newArray
    }

    reactions += {
      case event: UpdateInfo => infoupdate()
      case event: UpdateText => chatupdate()
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
