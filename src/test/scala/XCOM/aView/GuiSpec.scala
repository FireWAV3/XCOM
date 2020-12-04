package XCOM.aView
import XCOM.controller.Controller
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class GuiSpec extends WordSpec{
  val c = new Controller()
  val gui = Gui(c)
  "A Gui" should{
    "have a Constructor"in{
      val c = new Controller()
      val ui = UiTrait("GUI", c)
      ui shouldBe a [Gui]
    }
  }
}
