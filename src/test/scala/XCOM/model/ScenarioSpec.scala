package XCOM.model

import XCOM.model.Scenarios.json.ScenarioJson
import XCOM.model.Scenarios.xml.ScenarioXML
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class ScenarioSpec extends WordSpec{
  "A Scenario" should{
    "have a Constructor" in {
      val sC = new ScenarioXML()
      sC.getAmmount() should be(3)
      val sCj = new ScenarioJson()
      sCj.getAmmount() should be(3)
    }
    "have a methode loadScenario with x" in{
      new ScenarioXML().loadScenario(0) shouldBe a [Field]
      new ScenarioXML().loadScenario(1) shouldBe a [Field]
      new ScenarioXML().loadScenario(2) shouldBe a [Field]
      new ScenarioXML().loadScenario(3) shouldBe a [Field]

      new ScenarioJson().loadScenario(0) shouldBe a [Field]
      new ScenarioJson().loadScenario(1) shouldBe a [Field]
      new ScenarioJson().loadScenario(2) shouldBe a [Field]
      new ScenarioJson().loadScenario(3) shouldBe a [Field]
    }
  }
}
