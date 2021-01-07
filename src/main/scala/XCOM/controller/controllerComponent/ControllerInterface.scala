package XCOM.controller.controllerComponent

import XCOM.model.{AttackScenario, Field, TurnScenario}
import XCOM.model.PlayerStatus.PlayerStatus
import XCOM.util.UndoManager

import scala.swing.Publisher
import scala.util.Try

trait ControllerInterface extends Publisher{
  var field: Field
  var output: String
  var PlayerState: PlayerStatus
  var attack: AttackScenario
  var turnS: TurnScenario
  var context: Context
  var seed: Int
  var contextTravel: ContextTravel

  def deepCopy(): ControllerInterface
  def help: Unit
  def exit: Unit
  def info(str: Option[String]): Boolean
  def loadScenario(index: Int): Boolean
  def move(str: String, pX: Int, pY: Int): Boolean
  def aim(str1: Option[String], str2: Option[String]): Boolean
  def shoot(approval: Boolean): Boolean
  def next(): Boolean
  def undo(uManager: UndoManager): Unit
  def redo(uManager: UndoManager): Unit
  def fire(attHero: XCOM.model.Character, defHero: XCOM.model.Character): (Int, Int)
  def checkTurn(): Boolean
  def nextPlayerState(side: PlayerStatus): PlayerStatus
  def boundsX(x: Int): Boolean
  def boundsY(y: Int): Boolean
  def testRock(pX: Int, pY: Int): Boolean
  def testHero(pX: Int, pY: Int): Boolean
  def getHero(pX: Int, pY: Int): XCOM.model.Character
  def isHero(input: String): Option[XCOM.model.Character]
  def aStarMove(startX: Int, startY: Int, goalX: Int, goalY: Int): Boolean
  def movePossible(hero: XCOM.model.Character, pX: Int, pY: Int): Boolean
  def shootpercentage(attHero: XCOM.model.Character, defHero: XCOM.model.Character): Int
  def out(str: String): Unit
  def wrongInput(input: String): Unit
  def wrongGameState(): Unit
  def fieldToString: String
  def getCharacters: Vector[(String, Int, Int)]
  def getCharactersSide(hero: String): Int
  def getCharactersTypeIcon(hero: String): String
  def getRocks: Vector[(Int, Int)]
  def scenarioAmmount: Int
  def splitFlatString(input: String): Array[String]
  def testInt(input: String): Boolean
  def abcToInt(str: String): Int
  def testABC(str: String): Boolean
  def opponent(hero1: XCOM.model.Character, hero2: XCOM.model.Character): Boolean
  def scenarioAmmountTest(input: Try[Int]): Try[Boolean]
  def checkSide(side: Int): Boolean
  def helpOut: Unit
  def infoOut: Unit
  def requestRepaint: Unit
}
