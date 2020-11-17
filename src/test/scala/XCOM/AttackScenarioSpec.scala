package XCOM

import org.scalatest.Matchers._
import org.scalatest.WordSpec

class AttackScenarioSpec extends WordSpec{
  "An AttackScenario" should{
    "have an Attacker" in{
      AttackScenario(Character("Attacker",3,3,7,15,0,"A",new Cell()),new Character(), 30).attHero.name should be("Attacker")
    }
    "have a Defender" in{
      AttackScenario(new Character(),Character("Defender",3,3,7,15,0,"D",new Cell()), 30).defHero.name should be("Defender")
    }
    "have a Hitpercentage" in{
      AttackScenario(new Character(),new Character(),30).probability should be(30)
    }
  }
}
