//deprecated because of gui
package XCOM.util
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class ObservableSpec extends WordSpec{
  "An Observable" should{
    case class Observertest() extends Observer
    case class Observabletest() extends Observable

    val observer = Observertest()
    val observable = Observabletest()

    "have a methode add" in {
      observable.add(observer)
      observable.subscribers should be(Vector(observer))
    }

    "have a methode update" in {
      intercept[Exception] {observable.notifyObservers}
    }

    "have a methode remove" in {
      observable.remove(observer)
      observable.subscribers should be(Vector())
    }
  }
}

