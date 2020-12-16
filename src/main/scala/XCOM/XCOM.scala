package XCOM
import aView.UiTrait
import controller.Controller
import aView.gui._
import scala.util.{Failure, Success, Try}
import scala.io._

object XCOM {
  val c = new Controller()
  val uiType = "TUI"
  val ui = UiTrait(uiType, c)
  val gui = new SwingGUI(c)

  def main(args: Array[String]): Unit = {
    while(true){
      val input = StdIn.readLine().toUpperCase()
      Try(ui.run(input)) match {
        case Success(value) =>
        case Failure(exception) => print(exception)
      }
    }
  }
}