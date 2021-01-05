package XCOM.aView
import XCOM.controller.controllerComponent.controllerBaseImpl.Controller
import XCOM.util.UndoManager
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class GuiSpec extends WordSpec{
  val c = new Controller()
  val gui = Gui(c)
  "A Gui" should{
    val c = new Controller()
    val u = UndoManager()
    val ui = UiTrait("GUI", c, u)
    "have a Constructor"in{
      ui shouldBe a [Gui]
    }
    "have a methode run"in{
      intercept[Exception] {ui.run("")}
    }
    "have a methode update"in{
      val gui = Gui(c)
      intercept[Exception] {gui.update}
    }
  }
}

