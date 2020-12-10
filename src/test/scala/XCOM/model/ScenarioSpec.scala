package XCOM.model

import org.scalatest.Matchers._
import org.scalatest.WordSpec

class ScenarioSpec extends WordSpec{
  "A Scenario" should{
    "have a Constructor" in {
      val sC = Scenario()
      sC.amount should be(2)
    }
    "have a methode loadScenario with x" in{
      Scenario().loadScenario(0) shouldBe a [Field]
      Scenario().loadScenario(1) shouldBe a [Field]
      Scenario().loadScenario(2) shouldBe a [Field]
    }
    "have a methode loadScenario with 0" in{
      Scenario().loadScenario(-1).pX should be(1)
    }
  }
}
