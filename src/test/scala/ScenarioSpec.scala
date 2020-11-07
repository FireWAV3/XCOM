import XCOM.{Field, Scenario}
import org.scalatest.WordSpec
import org.scalatest.Matchers._

class ScenarioSpec extends WordSpec{
  "A Scenario" should{
    "have a methode loadScenario with x" in{
      Scenario().loadScenario(1) shouldBe a [Field]
    }
    "have a methode loadScenario with 0" in{
      Scenario().loadScenario(0).pX should be(1)
    }
  }
}
