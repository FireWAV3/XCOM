package XCOM.aView.gui

import XCOM.controller.Controller

import scala.swing.Swing.LineBorder
import scala.swing.event.MouseClicked
import scala.swing.{BorderPanel, BoxPanel, Dimension, Frame, GridPanel, Label, MainFrame, Orientation}
import scala.util.{Failure, Success, Try}

class GameField(c: Controller) extends Frame{


  def main = new MainFrame{
    title = "XCOM"


    var info = new BoxPanel(Orientation.Horizontal){
      var infoLabel =  new Label("Info"){
        minimumSize = new Dimension(1000, 100)
        preferredSize = new Dimension(1000, 100)
      }

      contents += infoLabel
    }

    var chat = new BoxPanel(Orientation.Vertical){
      var chatREDLabel =  new Label("CHAT RED"){
        minimumSize = new Dimension(200, 380)
        preferredSize = new Dimension(200, 380)
      }
      var chatBLUELabel =  new Label("CHAT BLUE"){
        minimumSize = new Dimension(200, 380)
        preferredSize = new Dimension(200, 380)
      }
      contents += chatREDLabel
        contents += chatBLUELabel
    }

    var cInput = ""
    var fieldList = List[Label]

    var field = new GridPanel(c.field.sizeY+1,c.field.sizeX+1){

      for( y <- 1 to c.field.sizeY+1){
        for( x <- 1 to c.field.sizeX+1){
          Try(c.testRock(x,y)) match {
            case Success(value) =>{
              Try(c.getHero(x,y)) match {
                case Success(value) =>{
                  fieldList += new Label(value.displayname){
                     background = java.awt.Color.YELLOW
                    border = LineBorder(java.awt.Color.YELLOW, 3)
                    foreground = java.awt.Color.BLACK

                  }
                }
                case Failure(exception) =>{
                  fieldList += new Label(){
                    val id = "X"
                    background = java.awt.Color.BLACK
                    border = LineBorder(java.awt.Color.BLACK, 1)
                  }
                }
              }
            }
            case Failure(exception) =>{
              fieldList += new Label("R"){
                val id = "R"
                background = java.awt.Color.GRAY
                border = LineBorder(java.awt.Color.GRAY, 3)
                foreground = java.awt.Color.BLACK

              }
            }
          }

        }
      }

      for( x <- fieldList){
        contents += x
      }




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
