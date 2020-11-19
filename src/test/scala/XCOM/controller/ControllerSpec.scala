package XCOM.controller
import GameState.{MENU, SUI}
import XCOM.model.{AttackScenario, Cell, Character, Field}
import org.scalatest.WordSpec
import XCOM.model.FieldStructure._
import org.scalatest.Matchers._

class ControllerSpec extends  WordSpec{
  "A Controller" should{
    var testField = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C))))
    var c = Controller(MENU, testField ,new AttackScenario())
    "have a methode boundsX" in{
      c.boundsX(0) should be(false)
      c.boundsX(5) should be(true)
      c.boundsX(6) should be(false)
    }
    "have a methode boundsY" in{
      c.boundsY(0) should be(false)
      c.boundsY(10) should be(true)
      c.boundsY(11) should be(false)
    }
    "have a methode testRock" in{
      c.testRock(5,5) should be(true)
      c.testRock(5,6) should be(false)
    }
    "have a methode testHero" in{
      c.testHero(6,6) should be(true)
      c.testHero(6,5) should be(false)
    }
    "have a methode isHero" in{
      c.isHero("C1")._1 should be(true)
      c.isHero("C2")._1 should be(false)
    }
    "have a methode movePossible" in{
      //down
      c.movePossible(testField.character(0),6,11) should be(true)
      c.movePossible(testField.character(0),6,12) should be(false)
      //up
      c.movePossible(testField.character(0),6,1) should be(true)
      c.movePossible(testField.character(0),6,0) should be(false)
      //right
      c.movePossible(testField.character(0),11,6) should be(true)
      c.movePossible(testField.character(0),12,6) should be(false)
      //left
      c.movePossible(testField.character(0),1,6) should be(true)
      c.movePossible(testField.character(0),0,6) should be(false)
      //diagonal left up
      c.movePossible(testField.character(0),4,4) should be(true)
      c.movePossible(testField.character(0),3,4) should be(true)
      c.movePossible(testField.character(0),4,3) should be(true)
      c.movePossible(testField.character(0),3,2) should be(false)
      c.movePossible(testField.character(0),2,3) should be(false)
      c.movePossible(testField.character(0),3,3) should be(false)
    }
    "have a methode shootpercentage" in{
      var shootingRange = new Field(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(1, 1, C)),
        Character("Tank", 5, 1, 70, 40, 0,"C2", Cell(2, 1, C)),Character("Assassin", 5, 2, 70, 40, 0,"C3", Cell(1, 2, C)),
        Character("Gunner", 5, 10, 70, 40, 0,"C4", Cell(2, 2, C))))
      c.shootpercentage(shootingRange.character(1),shootingRange.character(0)) should be(95)
      c.shootpercentage(shootingRange.character(2),shootingRange.character(1)) should be(20)
      c.shootpercentage(shootingRange.character(1),shootingRange.character(2)) should be(0)
      c.shootpercentage(shootingRange.character(2),shootingRange.character(0)) should be(20)
      c.shootpercentage(shootingRange.character(0),shootingRange.character(3)) should be(83)
    }
    "have a methode out" in{
      c.out("Test", SUI)
      c.output should be("Test")
      c.gameState should be(SUI)
    }
    "have a methode wrongInput" in{
      c.wrongInput("Error123")
      c.output should be("Wrong input: [Error123]")
    }
    "have a methode wrongGameState" in{
      c.wrongGameState()
      c.output should be ("You are not allowed to use that command right now")
    }
    "have a methode fieldToString" in{
      c.fieldToString should include("10")
      c.fieldToString should not include("11")
    }
    "have a methode scenarioAmmount" in{
      c.scenarioAmmount should be(1)
    }
    "have a methode splitFlatString" in{
      c.splitFlatString("a,b,c")(0) should be("a")
      c.splitFlatString("a,b,c")(1) should be("b")
      c.splitFlatString("a,b,c")(2) should be("c")
    }
    "have a methode testInt" in{
      c.testInt("123") should be(true)
      c.testInt("12a") should be(false)
    }
    "have a methode abcToInt" in{
      c.abcToInt("A") should be(1)
    }
    "have a methode testABC" in{
      c.testABC("Test") should be(false)
      c.testABC("E") should be(true)
      c.testABC("F") should be(false)
    }
  }
}
