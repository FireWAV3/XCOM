package XCOM
import aView.UiTrait
import aView.gui._
import util.UndoManager
import controller.controllerComponent.controllerBaseImpl.Controller

import scala.util.{Failure, Success, Try}
import scala.io._

object XCOM {
  var c = new Controller()
  val uManager = UndoManager()
  val uiType = "TUI"
  val ui = UiTrait(uiType, c, uManager)
 // val gui = new SwingGUI(c, uManager)

  def main(args: Array[String]): Unit = {
    while(true){
      val input = StdIn.readLine().toUpperCase()
      Try(ui.run(input)) match {
        case Success(value) =>
        case Failure(exception) => System.exit(0)
      }
    }
    val gui = new SwingGUI(c, uManager)
  }
}