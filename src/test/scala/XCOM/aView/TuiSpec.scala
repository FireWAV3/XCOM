package XCOM.aView

import XCOM.controller.Controller
import XCOM.controller.GameState._
import XCOM.model.AttackScenario
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
    "have a methode run" in{
      tui.run("LOAD 0")
      c.gameState should be(SUI)
      //
      tui.run("HELP")
      c.output should be("help")
      //
      tui.run("INFO C1")
      c.output should include("C1")
      //
      tui.run("MOVE C1 E 1")
      c.gameState should be(SUI)
      c.field.character(0).cell.x should be (4)
      c.field.character(0).cell.y should be (0)
      tui.run("MOVE C1 5 Z")
      c.output should be("Wrong input: [C1 5 Z]")
      //
      tui.run("SHOOT C1 C3")
      c.gameState should be(SHOOT)
      c.attack = new AttackScenario(c.field.character(0),c.field.character(2),100)
      tui.run("NO")
      c.gameState should be(SUI)

      tui.run("SHOOT C2 C3")
      val prevHP =   c.field.character(2).hp
      c.gameState should be(SHOOT)
      c.attack = new AttackScenario(c.field.character(1),c.field.character(2),100)
      tui.run("YES")
      (c.field.character(2).hp < prevHP) should be (true)

      tui.run("SHOOT C1 C3")
      c.gameState should be(SHOOT)
      c.attack = new AttackScenario(c.field.character(0),c.field.character(2),100)
      tui.run("YES")
      c.field.character should be(Vector[XCOM.model.Character](c.field.character(0),c.field.character(1)))
      //
      tui.run("GABAGUL")
      c.output should be("Wrong input: [GABAGUL]")
      //
      tui.run("EXIT")
      c.gameState should be(END)
    }
    "have a methode load" in{
      val c = new Controller()
      val tui = Tui(c)
      tui.run("LOAD -1")
      c.output should be("Wrong input: [-1]")
      tui.run("LOAD 0")
      c.gameState should be(SUI)
    }
  }
}
