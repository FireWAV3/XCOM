package XCOM.controller
import org.scalatest.Matchers._
import XCOM.controller.GameState.MENU
import XCOM.model.{AttackScenario, Field, Cell,Character}
import org.scalatest.WordSpec
import XCOM.model.FieldStructure._

class ControllerSpec extends  WordSpec{
  "A Controller" should{
    "have a methode testHero" in{
      val testField = new Field(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 1, C))))
      val c = Controller(MENU, testField ,new AttackScenario())
      c.isHero("C1") should be(true)
      c.isHero("C2") should be(false)
    }
  }
}
