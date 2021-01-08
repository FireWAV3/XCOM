package XCOM.controller.controllerComponent.controllerMockImpl

import XCOM.controller.controllerComponent.{Context, ContextTravel, ControllerInterface}
import XCOM.model
import XCOM.model.PlayerStatus.{BLUE, PlayerStatus}
import XCOM.model.{AttackScenario, Field, TurnScenario}
import XCOM.util.UndoManager

import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}

case class Controller(var field: Field, var attack: AttackScenario) extends ControllerInterface {

  override var output: String = ""
  override var PlayerState: PlayerStatus = BLUE
  override var turnS: TurnScenario = new TurnScenario
  override var context: Context = new Context(this)
  override var seed: Int = 0
  override var contextTravel: ContextTravel = new ContextTravel(this)


  def deepCopy(): ControllerInterface = this
  def help: Unit = {}
  def exit: Unit = {}
  def info(str: Option[String]): Boolean = false
  def loadScenario(index: Int): Boolean = false
  def move(str: String, pX: Int, pY: Int): Boolean = false
  def aim(str1: Option[String], str2: Option[String]): Boolean = false
  def shoot(approval: Boolean): Boolean = false
  def next(): Boolean = false
  def undo(uManager: UndoManager): Unit = {}
  def redo(uManager: UndoManager): Unit = {}
  def fire(attHero: XCOM.model.Character, defHero: XCOM.model.Character): (Int, Int) = (1,1)
  def checkTurn(): Boolean = false
  def nextPlayerState(side: PlayerStatus): PlayerStatus = BLUE
  def boundsX(x: Int): Boolean = false
  def boundsY(y: Int): Boolean = false
  def testRock(pX: Int, pY: Int): Boolean = false
  def testHero(pX: Int, pY: Int): Boolean = false
  def getHero(pX: Int, pY: Int): XCOM.model.Character = new model.Character()
  def isHero(input: String): Option[XCOM.model.Character] = Some(new model.Character())
  override def getFieldasArray(): Array[Array[Int]] = Array.ofDim[Int](1,1)
  def aStarMove(hero:model.Character, goalX: Int, goalY: Int): Boolean = false
  def movePossible(hero: XCOM.model.Character, pX: Int, pY: Int): Boolean = false
  def shootpercentage(attHero: XCOM.model.Character, defHero: XCOM.model.Character): Int = 1
  def out(str: String): Unit = {}
  def wrongInput(input: String): Unit = {}
  def wrongGameState(): Unit = {}
  def fieldToString: String = ""
  def getCharacters: Vector[(String, Int, Int)] = new ListBuffer[(String,Int,Int)]().toVector
  def getCharactersSide(hero: String): Int = 1
  def getCharactersTypeIcon(hero: String): String = "rifle"
  def getRocks: Vector[(Int, Int)] = new ListBuffer[(Int,Int)]().toVector
  def scenarioAmmount: Int = 1
  def splitFlatString(input: String): Array[String] = new Array[String](0)
  def testInt(input: String): Boolean = false
  def abcToInt(str: String): Int = 1
  def testABC(str: String): Boolean = false
  def opponent(hero1: XCOM.model.Character, hero2: XCOM.model.Character): Boolean = false
  def scenarioAmmountTest(input: Try[Int]): Try[Boolean] = Success(false)
  def helpOut: Unit = {}
  def infoOut: Unit = {}
  def requestRepaint: Unit = {}
  def checkSide(side: Int): Boolean = false
}
