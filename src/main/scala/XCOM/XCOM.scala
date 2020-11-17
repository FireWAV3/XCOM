package XCOM

import aView.Tui
import controller.Controller
import scala.io._

object XCOM {

  val c = new Controller()
  val tui = Tui(c)

  def main(args: Array[String]): Unit = {
    var running = true

    do{
      val input = StdIn.readLine().toUpperCase()
      running = tui.processInputLine(input)
    }while(running)
  }
}