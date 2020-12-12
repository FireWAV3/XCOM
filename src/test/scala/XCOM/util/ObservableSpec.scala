/*
package XCOM.util
import XCOM.aView.Tui
import XCOM.controller.Controller
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class ObservableSpec extends WordSpec{
  "A Observable" should{

    "have a remove methode" in {
      val c = new Controller()
      val tui = new Tui(c)
      c.add(tui)
      c.remove(tui)
      c.subscribers should be (Nil)

      class ObTest extends Observer
      val testOB = new ObTest
      c.add(testOB)
      intercept[Exception] {c.notifyObservers}
    }
  }
}
*/
