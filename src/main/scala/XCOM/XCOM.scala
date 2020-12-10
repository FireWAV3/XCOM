package XCOM
import aView.UiTrait
import controller.Controller

import scala.io._

object XCOM {
  val c = new Controller()

  val uiType = "TUI"
  val ui = UiTrait(uiType, c)
  c.notifyObservers

  def main(args: Array[String]): Unit = {
    while(true){
      val input = StdIn.readLine().toUpperCase()
      ui.run(input)
    }
  }
}