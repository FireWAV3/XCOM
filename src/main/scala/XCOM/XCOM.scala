package XCOM
import aView.UiTrait
import controller.Controller
import aView.gui._
import util.UndoManager

import scala.util.{Failure, Success, Try}
import scala.io._

object XCOM {
  var c = new Controller()
  val uManager = UndoManager()
  val uiType = "TUI"
  val ui = UiTrait(uiType, c, uManager)
  val gui = new SwingGUI(c, uManager)

  def main(args: Array[String]): Unit = {
    while(true){
      val input = StdIn.readLine().toUpperCase()
      Try(ui.run(input)) match {
        case Success(value) =>
        case Failure(exception) => System.exit(0)
      }
    }
  }
}