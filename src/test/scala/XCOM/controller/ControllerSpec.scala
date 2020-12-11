
package XCOM.controller
import XCOM.model.FieldStructure._
import XCOM.model.PlayerStatus._
import XCOM.model.{AttackScenario, Cell, Character, Field}
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class ControllerSpec extends  WordSpec{
  "A Controller" should{
    var testField = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C1", Cell(5, 6, C))))
    var c = Controller(testField ,new AttackScenario())
    "have a standard Constructor "in{
      Controller(testField ,new AttackScenario()).context.state shouldBe a [MenuState]
     }
    "have a default Constructor" in {
      val cC = new Controller()
      cC.context.state shouldBe a [MenuState]
      cC.field.sizeX should be(-1)
    }
     "have a methode help" in{
      c.help
      c.output should include("HELP")
    }

    "have a methode info" in{
      c.context.state = new SuiState(c)
      c.info(Some("AA")) should be(false)
      c.output should be("AA is not a Hero")
      c.info(Some("C1")) should be(true)
      c.output should include("C1")
      c.context.state = new MenuState(c)
      c.info(Some("C1")) should be(false)
      c.output should include("You are not allowed to use that command right now")
      c.info(None) should be(false)
    }
    "have a methode loadScenario" in{
      val cC = new Controller()
      cC.loadScenario(0) should be (true)
      cC.context.state shouldBe a [SuiState]
      cC.loadScenario(0) should be(false)
    }
    "have a methode move" in{
      var movementRange = new Field(5,10,Vector[Cell](Cell(3,3,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(4, 4, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(4, 5, C)),Character("SniperRED", 5, 10, 70, 40, 1,"C3", Cell(4, 3, C))))
      var cMove = Controller(movementRange ,new AttackScenario())
      cMove.turnS.load(0,movementRange)
      cMove.move("C1",5,7) should be(false)
      cMove.output should be("You are not allowed to use that command right now")
      cMove.context.state = new SuiState(cMove)
      cMove.move("C3",5,6) should be(false)
      cMove.output should include("Not a member of the Team that has the control")
      cMove.move("AA",5,7) should be(false)
      cMove.output should include("is not a valid Hero")
      cMove.move("C1",6,6) should be(false)
      cMove.output should include("Move not possible: not a tile on the field")
      cMove.move("C1",5,6) should be(false)
      cMove.output should include("Move not possible: There is another Hero at this position")
      cMove.move("C1",4,4) should be(false)
      cMove.output should include("Move not possible: There is a Rock at this position")
      cMove.move("C1",1,10) should be(false)
      cMove.output should include("Move not possible: Hero can't move this far")
      cMove.move("C1",5,7) should be(true)
      cMove.field.character should be(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(4, 6, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(4, 5, C)),Character("SniperRED", 5, 10, 70, 40, 1,"C3", Cell(4, 3, C))))
      cMove.move("C1",4,7) should be(false)
      cMove.output should include("Already moved")
      cMove.context.state = new ShootState(cMove)
      cMove.attack = AttackScenario(cMove.field.character(0),cMove.field.character(1),0)
      cMove.shoot(true)
      cMove.move("C1",5,7) should be(false)
      cMove.output should include("Already shot")

      cMove.contextTravel.travelState = new TravelStrategy(cMove)
      intercept[Exception] {cMove.contextTravel.travelState.movePossible(cMove.field.character(1),5,4)}

      cMove.contextTravel.travelState = new AStar(cMove)
      intercept[Exception] {cMove.contextTravel.travelState.movePossible(cMove.field.character(1),5,4)}

    }
    "have a methode aim" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),
        Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C)),Character("Assassin", 5, 10, 50, 90, 1,"C3", Cell(6, 6, C))))
      var cShoot = Controller(shootingRange ,new AttackScenario())
      cShoot.turnS.load(0,shootingRange)
      cShoot.aim(Some("C1"),Some("C2")) should be(true)
      cShoot.output should be("You are not allowed to use that command right now")
      cShoot.context.state = new SuiState(cShoot)
      cShoot.aim(Some("C3"),Some("C2")) should be(true)
      cShoot.output should include("Not a member of the Team that has the control")
      cShoot.aim(Some("AA"),Some("C2")) should be(true)
      cShoot.output should include("AA is not a valid Hero")
      cShoot.aim(Some("C1"),Some("C2")) should be(true)
      cShoot.output should include("Heros are on the same team")
      cShoot.aim(Some("C1"),Some("C3")) should be(true)
      cShoot.shoot(true)
      cShoot.aim(Some("C1"),Some("C3")) should be(true)
      cShoot.output should include("Already shot")
      cShoot.aim(Some("C1"),None) should be(false)
    }
    "have a methode shoot" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C)),Character("TankR", 5, 10, 50, 90, 1,"C3", Cell(5, 6, C))))
      var cShoot = Controller(shootingRange ,new AttackScenario())
      cShoot.turnS.load(0,shootingRange)
      cShoot.shoot(true) should be(false)
      cShoot.output should be("You are not allowed to use that command right now")
      cShoot.context.state = new ShootState(cShoot)
      cShoot.shoot(false) should be(false)
      cShoot.output should be("Shot canceled")
      cShoot.context.state = new ShootState(cShoot)
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(1),0)
      cShoot.shoot(true) should be(false)
      cShoot.output should include("Shot missed by ")
      cShoot.context.state = new ShootState(cShoot)
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(1),100)
      cShoot.shoot(true) should be(true)

      cShoot.context.state = new ShootState(cShoot)
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(2),100)
      cShoot.shoot(true) should be(true)
      cShoot.context.state = new ShootState(cShoot)
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(2),100)
      intercept[Exception] {cShoot.shoot(true) should be(true)}
    }
    "have a methode fire" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C))))
      var cShoot = Controller(shootingRange ,new AttackScenario())
      val attack1 = cShoot.fire(shootingRange.character(0),shootingRange.character(1))
      val attack2 = cShoot.fire(shootingRange.character(1),shootingRange.character(0))
      attack1._1 should be(70)
      attack1._2 should be(20)
      attack2._1 should be(50)
      attack2._2 should be(0)
      cShoot.field.character should be(Vector[Character](Character("Tank", 5, 10, 50, 20, 0,"C2", Cell(5, 6, C))))
    }
    "have a methode next" in{
      var sleepingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C))))
      var cSleep = Controller( sleepingRange ,new AttackScenario())
      cSleep.next should be(false)
      cSleep.output should be("You are not allowed to use that command right now")
      cSleep.context.state = new SuiState(cSleep)
      cSleep.next should be(true)
      cSleep.PlayerState should be(RED)
      cSleep.next should be(true)
      cSleep.PlayerState should be(BLUE)

    }
    "have a methode boundsX" in{
      intercept[Exception]{c.boundsX(0)}
      c.boundsX(5) should be(true)
      intercept[Exception] {c.boundsX(6)}
    }
    "have a methode boundsY" in{
      intercept[Exception] {c.boundsY(0)}
      c.boundsY(10) should be(true)
      intercept[Exception] {c.boundsY(11)}
    }
    "have a methode testRock" in{
      c.testRock(5,6) should be(true)
      intercept[Exception]{c.testRock(5,5)}
    }
    "have a methode testHero" in{
      c.testHero(6,5) should be(true)
      intercept[Exception]{c.testHero(6,6)}
    }
    "have a methode isHero" in{
      c.isHero("C1")shouldBe a [Some[Character]]
      c.isHero("C2")should be(None)
    }
    "have a methode aStarMove" in{
      intercept[Exception] {c.aStarMove(0,0,0,0)}
    }
    "have a methode movePossible" in{
      //down
      c.movePossible(testField.character(0),6,11) should be(true)
      intercept[Exception]{c.movePossible(testField.character(0),6,12)}

      //up
      c.movePossible(testField.character(0),6,1) should be(true)
      intercept[Exception] {c.movePossible(testField.character(0),6,0)}
      //right
      c.movePossible(testField.character(0),11,6) should be(true)
      intercept[Exception] {c.movePossible(testField.character(0),12,6)}
      //left
      c.movePossible(testField.character(0),1,6) should be(true)
      intercept[Exception] {c.movePossible(testField.character(0),0,6)}
      //diagonal left up
      c.movePossible(testField.character(0),4,4) should be(true)
      c.movePossible(testField.character(0),3,4) should be(true)
      c.movePossible(testField.character(0),4,3) should be(true)
      intercept[Exception] {c.movePossible(testField.character(0),3,2)}
      intercept[Exception] {c.movePossible(testField.character(0),2,3)}
      intercept[Exception] {c.movePossible(testField.character(0), 3, 3)}

      intercept[Exception] {c.movePossible(new Character("Nix_Name",0,0,0,1,-1,"Banane",new Cell()), 3, 3)}
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
      c.out("Test")
      c.output should be("Test")
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
      c.scenarioAmmount should be(2)
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

