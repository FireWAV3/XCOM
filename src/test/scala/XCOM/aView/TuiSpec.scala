package XCOM.aView

import XCOM.controller.Controller
import XCOM.controller.GameState._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class TuiSpec extends WordSpec{
  val c = new Controller()
  val tui = Tui(c)
  "A Tui" should{
    "have a Controller" in {
      tui.c shouldBe a [Controller]
    }
    "have a methode processInputLine" in {
      val tui2 = Tui(new Controller(END))
      tui.processInputLine("test") should be(true)
      tui2.processInputLine("test") should be(false)
    }
    "have a methode load" in{
      tui.run("LOAD 1")
      c.gameState should be(SUI)
      //
      tui.run("HELP")
      c.gameState should be(HELP)
      //
      tui.run("INFO C1")
      c.gameState should be(SINGLEOUT)
      //
      tui.run("EXIT")
      c.gameState should be(END)
    }
  }
}
