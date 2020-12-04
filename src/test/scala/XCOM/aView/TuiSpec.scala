
package XCOM.aView

import XCOM.controller._
import XCOM.controller.PlayerStatus._
import XCOM.model.AttackScenario
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class TuiSpec extends WordSpec{
  val c = new Controller()
  val tui = Tui(c)
  "A Tui" should{
    "have a Constructor" in{
      val c = new Controller()
      val tui = Tui(c)
      tui.c shouldBe a [Controller]
    }
    "have a methode run" in{
      tui.run("")
      c.context.state shouldBe a [MenuState]
      tui.run("     ")
      c.context.state shouldBe a [MenuState]
      tui.run(",,, ,,,")
      c.context.state shouldBe a [MenuState]
      //
      tui.run("LOAD")
      c.output should be("Wrong input: [LOAD]")
      tui.run("LOAD -1")
      c.output should be("Wrong input: [-1]")
      c.context.state shouldBe a [MenuState]
      tui.run("LOAD 0")
      c.context.state shouldBe a [SuiState]
      //
      tui.run("HELP")
      c.output should include("HELP")
      //
      tui.run("INFO C1")
      c.output should include("C1")
      tui.run("INFO XX")
      c.output should include("XX is not a Hero")
      //
      tui.run("NEXT")
      c.PlayerState should be (RED)
      tui.run("NEXT")
      c.PlayerState should be (BLUE)
      //
      tui.run("MOVE C1 E 1")
      c.context.state shouldBe a [SuiState]
      c.field.character(0).cell.x should be (4)
      c.field.character(0).cell.y should be (0)
      tui.run("MOVE C1 5 5")
      c.output should be("Wrong input: [C1 5 5]")
      tui.run("MOVE C1 E Z")
      c.output should be("Wrong input: [C1 E Z]")
      //
      tui.run("SHOOT C1 C3")
      c.context.state shouldBe a [ShootState]
      c.attack = new AttackScenario(c.field.character(0),c.field.character(2),100)
      tui.run("NO")
      c.context.state shouldBe a [SuiState]

      tui.run("SHOOT C2 C3")
      val prevHP =   c.field.character(2).hp
      c.context.state shouldBe a [ShootState]
      c.attack = new AttackScenario(c.field.character(1),c.field.character(2),100)
      tui.run("YES")
      (c.field.character(2).hp < prevHP) should be (true)

      tui.run("SHOOT C1 C3")
      c.context.state shouldBe a [ShootState]
      c.attack = new AttackScenario(c.field.character(0),c.field.character(2),100)
      tui.run("YES")
      c.field.character should be(Vector[XCOM.model.Character](c.field.character(0),c.field.character(1),c.field.character(2)))
      //
      tui.run("GABAGUL")
      c.output should be("Wrong input: [GABAGUL]")
      //
      //tui.run("EXIT")
    }
    "have a methode load" in{
      val c = new Controller()
      val tui = Tui(c)
      tui.run("LOAD -1")
      c.output should be("Wrong input: [-1]")
      tui.run("LOAD 0")
      c.context.state shouldBe a [SuiState]
    }
  }
}

