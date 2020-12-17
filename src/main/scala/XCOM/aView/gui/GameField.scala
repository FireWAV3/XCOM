package XCOM.aView.gui

import XCOM.controller.{Controller, UpdateField, UpdateInfo, UpdateShoot, UpdateText}
import XCOM.model.PlayerStatus._
import javax.swing.Icon

import scala.collection.mutable.ListBuffer
import scala.swing.Swing.{EmptyIcon, LineBorder}
import scala.swing.event.MouseClicked
import scala.swing.{BorderPanel, BoxPanel, Dialog, Dimension, Frame, GridPanel, Label, MainFrame, Orientation}
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
    var highlightedCell = Array[IdLabel](new IdLabel(""), new IdLabel(""))


    var field = new GridPanel(c.field.sizeY+1,c.field.sizeX+1){
      var cells = Array.ofDim[IdLabel](c.field.sizeX+1,c.field.sizeY+1)
      for( y <- 1 to c.field.sizeY+1){
        for( x <- 1 to c.field.sizeX+1){
          Try(c.testRock(x,y)) match {
            case Success(value) =>{
              Try(c.getHero(x,y)) match {
                case Success(value) =>{
                  val heroLabel = new IdLabel(value.displayname){
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
                            c.move(cell.id,place._1+1,place._2+1)
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
                case Failure(exception) =>{
                  val emptyLabel = new IdLabel("X"){
                    recolor(this)
                    listenTo(mouse.clicks)
                    reactions +={
                      case MouseClicked(scr,pt,mod,clicks,pops) => {
                        mod match {
                          case 0 => {
                            if(id.contains("C")) {
                              highlightedCell(0) = highlightedCell(1)
                              highlightedCell(1) = this
                              c.info(Some(id))
                            }
                          }
                          case 256 => { //right Mousebutton
                            val cell = highlightedCell(1)
                            val place = findCell(this)
                            c.move(cell.id,place._1+1,place._2+1)
                          }
                          case _ => { //middle Mousebutton
                            val cell = highlightedCell(1)
                            c.aim(Some(cell.id),Some(this.id))
                          }
                        }
                      }
                    }
                  }
                  cells(x-1)(y-1) = emptyLabel
                  contents += emptyLabel
                }
              }
            }
            case Failure(exception) =>{
              val rockLabel = new IdLabel("R"){
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
                        c.move(cell.id,place._1+1,place._2+1)
                      }
                      case _ => { //middle Mousebutton
                        val cell = highlightedCell(1)
                        c.aim(Some(cell.id),Some(this.id))
                      }
                    }
                  }
                }
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


    def shootupdate() = {
      //TODO
      val res = Dialog.showConfirmation(_,c.output,"Shoot?",Dialog.Options.YesNo,_,_)

      if(res == Dialog.Result.Yes){
        c.shoot(true)
      }else{
        c.shoot(false)
      }
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
          cell.text = ""
          cell.background = java.awt.Color.BLACK
          cell.border = LineBorder(java.awt.Color.BLACK, 1)
        }
        case "R" => {
          cell.text = cell.id
          cell.background = java.awt.Color.GRAY
          cell.border = LineBorder(java.awt.Color.GRAY, 3)
        }
        case _ => {
          cell.text = cell.id
          cell.background = java.awt.Color.YELLOW
          cell.border = LineBorder(java.awt.Color.YELLOW, 3)
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