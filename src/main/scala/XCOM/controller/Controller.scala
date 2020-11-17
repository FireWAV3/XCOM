package XCOM.controller
import XCOM.model
import XCOM.model.{AttackScenario, Cell, Character, Field, Scenario}
import XCOM.model.FieldStructure._

import scala.collection.mutable.ListBuffer

object GameState extends Enumeration {
type GameState = Value
  val MENU, SUI, SHOOT, END , HELP = Value
}
import GameState._

case class Controller(var gameState: GameState,var field: Field, var attack : AttackScenario){

  def this (){
    this(MENU, new Field(0,0),new AttackScenario())
  }

  def laodScenario(index:Int): (Field,Int) ={
    val scenario = Scenario()
    (scenario.loadScenario(index.toInt),scenario.amount)
  }

  def move(hero:model.Character, cGameField:Field, pX:Int, pY:Int):Field = {
    var newCharacterV = ListBuffer[model.Character]()
    for(e <- cGameField.character){
      if(e.displayname == hero.displayname){
        newCharacterV += Character(e.name,e.mrange,e.srange,e.damage,e.hp,e.side,e.displayname,Cell(pX-1, pY-1, C))
      }else{
        newCharacterV += e
      }
    }
    Field(cGameField.pX,cGameField.pY,cGameField.rocks,newCharacterV.toVector)
  }

  def testRock(cGameField:Field, pX: Int, pY: Int): Boolean = {
    for(e <- cGameField.rocks if e.x == (pX-1)  if e.y == (pY-1) ) return true
    false
  }

  def testHero(cGameField: Field, pX: Int, pY: Int): Boolean = {
    for(e <- cGameField.character if e.cell.x == pX-1 && e.cell.y == pY-1)return true
    false
  }

  def movePossible(hero:model.Character, cGameField:Field, pX:Int, pY:Int):Boolean = {
    //TODO A*
    val xDistance = pX - 1 - hero.cell.x
    val yDistance = pY - 1 - hero.cell.y
    var distance = 0
    if (xDistance < 0 && yDistance < 0){
      distance = -xDistance - yDistance
    } else if (yDistance < 0){
      distance = xDistance - yDistance
    } else if (xDistance < 0){
      distance = -xDistance + yDistance
    } else {
      distance = xDistance + yDistance
    }
    hero.mrange >= distance
  }

  def shootpercentage(cGameField: Field, attHero: model.Character, defHero: model.Character): Int ={
    val xDistance = attHero.cell.x - defHero.cell.x
    val yDistance = attHero.cell.y - defHero.cell.y
    var distance = 0
    if (xDistance < 0  && yDistance < 0){
      distance = -xDistance - yDistance
    } else if (yDistance < 0){
      distance = xDistance - yDistance
    } else if (xDistance < 0){
      distance = -xDistance + yDistance
    } else {
      distance = xDistance + yDistance
    }
    if (distance > attHero.srange){
      return 0
    }
    val minPercentage = 20
    val maxPercentage = 99
    if (attHero.srange == 1){
      return 95
    }
    val hitChance = maxPercentage - (((maxPercentage-minPercentage)/(attHero.srange-1))*distance)
    if (hitChance < 20){
      return 20
    }
    hitChance
  }

  def fire(cGameField:Field, attHero: model.Character, defHero: model.Character): (Field,Int,Int) ={
    var newCharacterV = ListBuffer[model.Character]()
    for(e <- cGameField.character){
      if(e.displayname == defHero.displayname){
        if (defHero.hp - attHero.damage > 0)
          newCharacterV += Character(e.name,e.mrange,e.srange,e.damage,e.hp-attHero.damage,e.side,e.displayname,e.cell)
      }else{
        newCharacterV += e
      }
    }
    (Field(cGameField.pX,cGameField.pY,cGameField.rocks,newCharacterV.toVector),attHero.damage,if ((defHero.hp-attHero.damage)>0) (defHero.hp-attHero.damage) else 0)
  }

  def splitFlatString(input:String):Array[String] = {
    input.replace(',',' ').split("\\s+")
  }

  def testInt(input:String):Boolean = {
    input.forall(_.isDigit)
  }

  def testABC(cGameField:Field, str: String):Boolean = {
    if(str.length == 1){
      val chr = str.charAt(0)
      if(chr >= 'A' && chr <= 'A'+cGameField.sizeX){
        return true
      }
    }
    false
  }

  def abctoInt(str: String):Int = {
    val chr = str.charAt(0)
    chr - 'A' +1
  }
}
