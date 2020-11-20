package XCOM.model

import XCOM.model
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class AttackScenarioSpec extends WordSpec{
  "An AttackScenario" should{
    "have an Attacker" in{
      AttackScenario(Character("Attacker",3,3,7,15,0,"A",new Cell()),new model.Character(), 30).attHero.name should be("Attacker")
    }
    "have a Defender" in{
      AttackScenario(new model.Character(),Character("Defender",3,3,7,15,0,"D",new Cell()), 30).defHero.name should be("Defender")
    }
    "have a Hitpercentage" in{
      AttackScenario(new model.Character(),new model.Character(),30).probability should be(30)
    }
    "have an empty Constructor" in{
      val attack = new AttackScenario()
      attack.attHero.name should be("Test")
      attack.defHero.name should be("Test")
      attack.probability should be(0)
    }
  }
}
