package XCOM.controller
import XCOM.controller.controllerComponent.controllerBaseImpl.Controller
import XCOM.controller.controllerComponent._
import XCOM.model.FieldStructure._
import XCOM.model.PlayerStatus._
import XCOM.model.{AttackScenario, Cell, Character, Field, TurnScenario}
import XCOM.util.UndoManager
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.util.Success


class ControllerSpec extends  WordSpec{
  "A Controller" should{
    var testField = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C))))
    var c = Controller(testField ,new AttackScenario())
    "have a standard Constructor "in{
      controllerBaseImpl.Controller(testField ,new AttackScenario()).context.state shouldBe a [MenuState]
     }
    "have a default Constructor" in {
      val cC = new Controller()
      cC.context.state shouldBe a [MenuState]
      cC.field.sizeX should be(-1)
    }
     "have a methode help" in{
      c.help
      c.context.state shouldBe a [MenuState]
    }

    "have a methode info" in{
      c.context.state = new SuiState(c)
      c.info(Some("AA")) should be(false)
      c.output should be("Who do you mean? We don't know AA")
      c.info(Some("C1")) should be(true)
      c.output should include("C1")
      c.context.state = new MenuState(c)
      c.info(Some("C1")) should be(false)
      c.output should be("We should focus on other problems first")
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
      var cMove = controllerBaseImpl.Controller(movementRange ,new AttackScenario())
      cMove.turnS.load(0,movementRange)
      cMove.move("C1",5,7) should be(false)
      cMove.output should be("We should focus on other problems first")
      cMove.context.state = new SuiState(cMove)
      cMove.move("C3",5,6) should be(false)
      cMove.output should include("Why would I listen to you? You're my enemy")
      cMove.move("AA",5,7) should be(false)
      cMove.output should be("Who do you mean? We don't know AA")
      cMove.move("C1",6,6) should be(false)
      cMove.output should be("What is this place you're talking of?")
      cMove.move("C1",5,6) should be(false)
      cMove.output should be("I can't stand on his head, can I?")
      cMove.move("C1",4,4) should be(false)
      cMove.output should be("This rock's to high to climb it")
      cMove.move("C1",1,10) should be(false)
      cMove.output should be("That's way too far away. I would never get there")
      cMove.move("C1",5,7) should be(true)
      cMove.field.character should be(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(4, 6, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(4, 5, C)),Character("SniperRED", 5, 10, 70, 40, 1,"C3", Cell(4, 3, C))))
      cMove.move("C1",4,7) should be(false)
      cMove.output should include("Man, I'm to exhausted to go anywhere right now")
      cMove.context.state = new ShootState(cMove)
      cMove.attack = AttackScenario(cMove.field.character(0),cMove.field.character(1),0)
      cMove.shoot(true)
      cMove.move("C1",5,7) should be(false)
      cMove.output should include("Need some time to reload first")

      cMove.contextTravel.travelState = new TravelStrategy(cMove)
      intercept[Exception] {cMove.contextTravel.travelState.movePossible(cMove.field.character(1),5,4)}
    }
    "have a methode aim" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),
        Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C)),Character("Assassin", 5, 10, 50, 90, 1,"C3", Cell(6, 6, C))))
      var cShoot = controllerBaseImpl.Controller(shootingRange ,new AttackScenario())
      cShoot.turnS.load(0,shootingRange)
      cShoot.aim(Some("C1"),Some("C2")) should be(true)
      cShoot.output should be("We should focus on other problems first")
      cShoot.context.state = new SuiState(cShoot)
      cShoot.aim(Some("C3"),Some("C2")) should be(true)
      cShoot.output should be("Why would I listen to you? You're my enemy")
      cShoot.aim(Some("AA"),Some("C2")) should be(true)
      cShoot.output should be("Who do you mean? We don't know AA")
      cShoot.aim(Some("C1"),Some("C2")) should be(true)
      cShoot.output should be("Are you blind? Why should I shoot my mate?")
      cShoot.aim(Some("C1"),Some("C3")) should be(true)
      cShoot.shoot(true)
      cShoot.aim(Some("C1"),Some("C3")) should be(true)
      cShoot.output should include("Need some time to reload first")
      cShoot.aim(Some("C1"),None) should be(false)
    }
    "have a methode shoot" in{
      var shootingRange = new Field(5,10,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C)),Character("TankR", 5, 10, 50, 90, 1,"C3", Cell(5, 6, C))))
      var cShoot = controllerBaseImpl.Controller(shootingRange ,new AttackScenario())
      cShoot.turnS.load(0,shootingRange)
      cShoot.shoot(true) should be(false)
      cShoot.output should be("We should focus on other problems first")
      cShoot.context.state = new ShootState(cShoot)
      cShoot.shoot(false) should be(false)
      cShoot.output should be("Affirmative. Target unlocked, safety back on")
      cShoot.context.state = new ShootState(cShoot)
      cShoot.attack = AttackScenario(cShoot.field.character(0),cShoot.field.character(1),0)
      cShoot.shoot(true) should be(false)
      cShoot.output should include("Damnit I missed him. Would have taken only")
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
      var cShoot = controllerBaseImpl.Controller(shootingRange ,new AttackScenario())
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
      var cSleep = controllerBaseImpl.Controller( sleepingRange ,new AttackScenario())
      cSleep.next should be(false)
      cSleep.output should be("We should focus on other problems first")
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
    "have a methode getHero" in{
      c.getHero(6,6) shouldBe a [Character]
      intercept[Exception]{c.getHero(6,5)}
    }
    "have a methode isHero" in{
      c.isHero("C1")shouldBe a [Some[Character]]
      c.isHero("C3")should be(None)
    }
    "have a methode aStarMove" in{
      //intercept[Exception] {c.aStarMove(0,0,0,0)}
    }
    "have a methode movePossible" in{
      var moveField = Field(15,20,Vector[Cell](Cell(4,4,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, -1,"C1", Cell(5, 5, C)),Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C))))
      var cMove = Controller(moveField ,new AttackScenario())


      //down
      cMove.movePossible(moveField.character(0),6,11) should be(true)
      intercept[Exception]{cMove.movePossible(moveField.character(0),6,12)}
      cMove.movePossible(moveField.character(1),6,12) should be(true)
      intercept[Exception]{cMove.movePossible(moveField.character(1),6,13)}

      //up
      cMove.movePossible(moveField.character(0),6,1) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(0),6,0)}
      cMove.movePossible(moveField.character(1),6,4) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(1),6,3)}
      //right
      cMove.movePossible(moveField.character(0),11,6) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(0),12,6)}
      cMove.movePossible(moveField.character(1),11,7) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(1),12,7)}
      //left
      cMove.movePossible(moveField.character(0),1,6) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(0),0,6)}
      cMove.movePossible(moveField.character(1),1,7) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(1),0,7)}
      //diagonal left up
      cMove.movePossible(moveField.character(0),4,4) should be(true)
      cMove.movePossible(moveField.character(0),3,4) should be(true)
      cMove.movePossible(moveField.character(0),4,3) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(0),3,2)}
      intercept[Exception] {cMove.movePossible(moveField.character(0),2,3)}
      intercept[Exception] {cMove.movePossible(moveField.character(0), 3, 3)}
      cMove.movePossible(moveField.character(1),4,5) should be(true)
      cMove.movePossible(moveField.character(1),3,5) should be(true)
      cMove.movePossible(moveField.character(1),4,4) should be(true)
      intercept[Exception] {cMove.movePossible(moveField.character(1),3,3)}
      intercept[Exception] {cMove.movePossible(moveField.character(1),2,4)}
      intercept[Exception] {cMove.movePossible(moveField.character(1), 3, 4)}
    }
    "have a methode shootpercentage" in{
      var shootingRange = Field(200,200,Vector[Cell](Cell(2,80,R),Cell(3,81,R)),Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(1, 1, C)),
        Character("Tank", 5, 1, 70, 40, 0,"C2", Cell(2, 1, C)),Character("Assassin", 5, 3, 70, 40, 0,"C3", Cell(1, 3, C)),
        Character("Gunner", 5, 10, 70, 40, 0,"C4", Cell(2, 2, C)),Character("Longrange", 5, 80, 70, 40, 0,"C5", Cell(2, 81, C)),
        Character("Target", 5, 10, 70, 40, 0,"C6", Cell(5, 81, C)),Character("Victim", 5, 10, 70, 40, 0,"C7", Cell(5, 78, C)),
        Character("Meelee", 5, 2, 70, 40, 0,"C8", Cell(1, 5, C)),Character("Blob", 5, 1, 70, 40, 0,"C9", Cell(50, 50, C)),
        Character("Blub", 5, 2, 70, 40, 0,"C8", Cell(51, 52, C))))
      var cShoot = Controller(shootingRange,new AttackScenario())
      cShoot.shootpercentage(shootingRange.character(1),shootingRange.character(0)) should be(95)
      cShoot.shootpercentage(shootingRange.character(8),shootingRange.character(9)) should be(0)
      cShoot.shootpercentage(shootingRange.character(2),shootingRange.character(1)) should be(0)
      cShoot.shootpercentage(shootingRange.character(1),shootingRange.character(2)) should be(0)
      cShoot.shootpercentage(shootingRange.character(2),shootingRange.character(0)) should be(60)
      cShoot.shootpercentage(shootingRange.character(0),shootingRange.character(3)) should be(91)
      cShoot.shootpercentage(shootingRange.character(7),shootingRange.character(2)) should be(20)
      cShoot.shootpercentage(shootingRange.character(4),shootingRange.character(5)) should be(96)
      cShoot.shootpercentage(shootingRange.character(4),shootingRange.character(3)) should be(20)
      cShoot.shootpercentage(shootingRange.character(5),shootingRange.character(6)) should be(83)
    }
    "have a methode out" in{
      c.out("Test")
      c.output should be("Test")
    }
    "have a methode wrongInput" in{
      c.wrongInput("Error123")
      c.output should be("What are you trying to say with [Error123]?")
    }
    "have a methode wrongGameState" in{
      c.wrongGameState()
      c.output should be ("We should focus on other problems first")
    }
    "have a methode fieldToString" in{
      c.fieldToString should include("10")
      c.fieldToString should not include("11")
    }
    "have a methode getCharacters" in{
      c.getCharacters should be(Vector(("C1",5,5),("C2",5,6)))
    }
    "have a methode getCharactersSide" in{
      c.getCharactersSide("C1") should be(0)
      c.getCharactersSide("AA") should be(-1)
    }
    "have a methode getCharactersTypeIcon" in{
      val paradeRange = new Field(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 5, C)),
        Character("Tank", 5, 10, 50, 90, 0,"C2", Cell(5, 6, C)),Character("Assassin", 5, 10, 50, 90, 1,"C3", Cell(5, 7, C)),
        Character("Assassin Nr.2", 5, 10, 50, 90, 0,"C4", Cell(5, 8, C)),Character("Ghost", 5, 10, 50, 90, 1,"C5", Cell(5, 9, C))))
      val cParade = controllerBaseImpl.Controller(paradeRange, new AttackScenario())
      cParade.getCharactersTypeIcon("C1") should be("sniper")
      cParade.getCharactersTypeIcon("C2") should be("tank")
      cParade.getCharactersTypeIcon("C3") should be("shotgun")
      cParade.getCharactersTypeIcon("C4") should be("knife")
      cParade.getCharactersTypeIcon("C5") should be("rifle")
      cParade.getCharactersTypeIcon("AA") should be("")
    }
    "have a methode getRocks" in{
      c.getRocks should be (Vector((4,4)))
    }
    "have a methode scenarioAmmount" in{
      c.scenarioAmmount should be(3)
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
    "have a methode scenarioAmmountTest" in{
      c.scenarioAmmountTest(Success(0)) should be(Success(true))
    }
    "have a methode requestRepaint" in{
      c.requestRepaint
      c.context.state shouldBe a[MenuState]
    }
    "have a stub implementation" in{
      val mock = controllerComponent.controllerMockImpl.Controller(testField,new AttackScenario())
      val uManager = UndoManager()
      mock.output should be("")
      mock.PlayerState should be(BLUE)
      mock.turnS shouldBe a[TurnScenario]
      mock.context shouldBe a[Context]
      mock.seed should be(0)
      mock.contextTravel shouldBe a[ContextTravel]

      mock.deepCopy() should be(mock)
      mock.help
      mock.exit
      mock.info(Some("")) should be(false)
      mock.loadScenario(0) should be(false)
      mock.move("",0,0) should be(false)
      mock.aim(Some(""),Some("")) should be(false)
      mock.shoot(false) should be(false)
      mock.next() should be(false)
      mock.undo(uManager)
      mock.redo(uManager)
      mock.fire(new Character(),new Character()) should be((1,1))
      mock.checkTurn() should be(false)
      mock.nextPlayerState(BLUE) should be(BLUE)
      mock.boundsX(0) should be(false)
      mock.boundsY(0) should be(false)
      mock.testRock(0,0) should be(false)
      mock.testHero(0,0) should be(false)
      mock.getHero(0,0) shouldBe a[Character]
      mock.isHero("") shouldBe a[Some[Character]]
      mock.getFieldasArray() should be(Array.ofDim[Int](1,1))
      mock.aStarMove(new Character(),0,0) should be(false)
      mock.movePossible(new Character(),0,0) should be(false)
      mock.shootpercentage(new Character(), new Character()) should be(1)
      mock.out("")
      mock.wrongInput("")
      mock.wrongGameState()
      mock.fieldToString should be("")
      mock.getCharacters shouldBe a[Vector[(String,Int,Int)]]
      mock.getCharactersSide("") should be(1)
      mock.getCharactersTypeIcon("") should be("rifle")
      mock.getRocks shouldBe a[Vector[(Int, Int)]]
      mock.scenarioAmmount should be(1)
      mock.splitFlatString("") shouldBe a[Array[String]]
      mock.testInt("") should be(false)
      mock.abcToInt("") should be(1)
      mock.testABC("") should be(false)
      mock.opponent(new Character(), new Character()) should be(false)
      mock.scenarioAmmountTest(Success(0)) should be(Success(false))
      mock.helpOut
      mock.infoOut
      mock.requestRepaint
      mock.checkSide(0) should be(false)
    }
  }
}

