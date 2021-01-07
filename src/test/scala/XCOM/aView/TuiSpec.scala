package XCOM.aView
import XCOM.controller.controllerComponent.controllerBaseImpl.Controller
import XCOM.controller.controllerComponent.{MenuState, ShootState, SuiState}
import XCOM.model.PlayerStatus._
import XCOM.model.AttackScenario
import XCOM.util.UndoManager
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class TuiSpec extends WordSpec{
  val c = new Controller()
  val uManager = UndoManager()
  val tui = Tui(c, uManager)
  "A Tui" should{
    "have a Constructor" in{
      val c = new Controller()
      val ui = UiTrait("TUI", c, uManager)
      ui shouldBe a [Tui]
    }
    "have a methode run" in{
      intercept[Exception] {tui.run("EXIT")}
      tui.run("")
      c.context.state shouldBe a [MenuState]
      tui.run("     ")
      c.context.state shouldBe a [MenuState]
      tui.run(",,, ,,,")
      c.context.state shouldBe a [MenuState]
      //
      tui.run("LOAD")
      c.output should be("What are you trying to say with [LOAD]?")
      tui.run("LOAD A")
      c.output should be("What are you trying to say with [LOAD A]?")
      tui.run("LOAD -1")
      c.output should be("What are you trying to say with [LOAD -1]?")
      c.context.state shouldBe a [MenuState]
      tui.run("LOAD 0")
      c.context.state shouldBe a [SuiState]
      //
      tui.run("HELP")
      c.context.state shouldBe a [SuiState]
      //
      tui.run("INFO C1")
      c.output should include("C1")
      tui.run("INFO XX")
      c.output should be("What are you trying to say with [INFO XX]?")
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
      //
      tui.run("")
      c.output should be("What are you trying to say with []?")
      tui.run("MOVE C1 E")
      c.output should be("What are you trying to say with [MOVE C1 E]?")
      tui.run("MOVE C1 5 5")
      c.output should be("What are you trying to say with [MOVE C1 5 5]?")
      tui.run("MOVE C1 E Z")
      c.output should be("What are you trying to say with [MOVE C1 E Z]?")
      //
      tui.run("UNDO")
      c.context.state shouldBe a [SuiState]
      c.field.character(0).cell.x should be (5)
      c.field.character(0).cell.y should be (1)

      tui.run("REDO")
      c.context.state shouldBe a [SuiState]
      c.field.character(0).cell.x should be (4)
      c.field.character(0).cell.y should be (0)
      //
      tui.run("SHOOT C1")
      c.output should be("What are you trying to say with [SHOOT C1]?")

      tui.run("SHOOT C1 C9")
      c.output should be("Who do you mean? I don't know C9")

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
      c.output should be("What are you trying to say with [GABAGUL]?")
      //
      //tui.run("EXIT")
    }
    "have a methode load" in{
      val c = new Controller()
      val uManager = new UndoManager
      val tui = Tui(c, uManager)
      tui.run("LOAD -1")
      c.output should be("What are you trying to say with [LOAD -1]?")
      tui.run("LOAD 0")
      c.context.state shouldBe a [SuiState]
    }
  }
}

