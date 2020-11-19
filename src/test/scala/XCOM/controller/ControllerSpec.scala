package XCOM.controller
import GameState.{MENU, SUI, SHOOT,END}
import XCOM.model.{AttackScenario, Cell, Character, Field}
import org.scalatest.WordSpec
import XCOM.model.FieldStructure._
import org.scalatest.Matchers._

class ControllerSpec extends  WordSpec{
  "A Controller" should{
    var testField = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C1", Cell(5, 6, C))))
    var c = Controller(MENU, testField ,new AttackScenario())
    "have a standard Constructor "in{
      Controller(MENU, testField ,new AttackScenario()).gameState should be(MENU)
     }
    "have a default Constructor" in {
      val cC = new Controller()
      cC.gameState should be(MENU)
      cC.field.sizeX should be(-1)
    }
    "have a gameState Constructor" in {
      val cC = new Controller(END)
      cC.gameState should be(END)
    }
    "have a methode help" in{
      c.help
      c.output should be("help")
    }
    "have a methode exit" in{
      val cC = new Controller()
      cC.exit
      cC.gameState should be(END)
    }
    "have a methode info" in{
      c.info("AA") should be(false)
      c.output should be("AA is not a Hero")
      c.info("C1") should be(true)
      c.output should include("C1")
    }
    "have a methode loadScenario" in{
      val cC = new Controller()
      cC.loadScenario(0) should be (true)
      cC.gameState = SUI
      cC.loadScenario(0) should be(false)
    }
    "have a methode move" in{
      var movementRange = new Field(5,10,Vector[Cell](Cell(3,3,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(4, 4, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(4, 5, C))))
      var cMove = Controller(MENU, movementRange ,new AttackScenario())
      cMove.move("C1",5,7) should be(false)
      cMove.output should be("You are not allowed to use that command right now")
      cMove.gameState = SUI
      cMove.move("AA",5,7) should be(false)
      cMove.output should include("is not a valid Hero")
      cMove.move("C1",6,6) should be(false)
      cMove.output should be("Move not possible: not a tile on the field")
      cMove.move("C1",5,6) should be(false)
      cMove.output should be("Move not possible: There is another object at this position")
      cMove.move("C1",4,4) should be(false)
      cMove.output should be("Move not possible: There is another object at this position")
      cMove.move("C1",1,10) should be(false)
      cMove.output should be("Move not possible: Hero can't move this far")
      cMove.move("C1",5,7) should be(true)
      cMove.field.character should be(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(4, 6, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(4, 5, C))))
    }
    "have a methode aim" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),
        Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C)),Character("Assassin", 5, 10, 50, 90, 1,"C3", Cell(6, 6, C))))
      var cShoot = Controller(MENU, shootingRange ,new AttackScenario())
      cShoot.aim("C1","C2") should be(false)
      cShoot.output should be("You are not allowed to use that command right now")
      cShoot.gameState = SUI
      cShoot.aim("AA","C2") should be(false)
      cShoot.output should be("Please enter 2 valid heros")
      cShoot.aim("C1","C2") should be(false)
      cShoot.output should be("Heros are on the same team")
      cShoot.aim("C1","C3") should be(true)
    }
    "have a methode shoot" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C))))
      var cShoot = Controller(MENU, shootingRange ,new AttackScenario())
      cShoot.shoot(true) should be(false)
      cShoot.output should be("You are not allowed to use that command right now")
      cShoot.gameState = SHOOT
      cShoot.shoot(false) should be(false)
      cShoot.output should be("Shot canceled")
      cShoot.gameState = SHOOT
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(1),0)
      cShoot.shoot(true) should be(false)
      cShoot.output should include("Shot missed by ")
      cShoot.gameState = SHOOT
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(1),100)
      cShoot.shoot(true) should be(true)
    }
    "have a methode fire" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C))))
      var cShoot = Controller(MENU, shootingRange ,new AttackScenario())
      val attack1 = cShoot.fire(shootingRange.character(0),shootingRange.character(1))
      val attack2 = cShoot.fire(shootingRange.character(1),shootingRange.character(0))
      attack1._1 should be(70)
      attack1._2 should be(20)
      attack2._1 should be(50)
      attack2._2 should be(0)
      cShoot.field.character should be(Vector[Character](Character("Tank", 5, 10, 50, 20, 0,"C2", Cell(5, 6, C))))
    }
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
