package XCOM.aView

import XCOM.controller.Controller
import XCOM.controller.GameState._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class TuiSpec extends WordSpec{
  val tui = Tui(new Controller())
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

    }
  }
}
