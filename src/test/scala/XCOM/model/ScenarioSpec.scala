package XCOM.model

import org.scalatest.Matchers._
import org.scalatest.WordSpec

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
