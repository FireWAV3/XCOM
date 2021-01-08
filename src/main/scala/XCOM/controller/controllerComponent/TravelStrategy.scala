package XCOM.controller.controllerComponent

import XCOM.model

trait TravelStrategyTrait{
  def movePossible(hero:model.Character, pX:Int, pY:Int):Boolean
}

class ContextTravel(c:ControllerInterface){
  def deepCopy(): ContextTravel = {
    var CTout = new ContextTravel(this.c)
    CTout.travelState = this.travelState
    CTout
  }

  var travelState:TravelStrategyTrait = new Manhattan(c)
}

class TravelStrategy (c:ControllerInterface) extends TravelStrategyTrait {
  override def movePossible(hero: model.Character, pX: Int, pY: Int): Boolean = throw new Exception("wtf how?")
}

class AStar(c:ControllerInterface)extends TravelStrategy(c){
  override def movePossible(hero: model.Character, pX: Int, pY: Int): Boolean = {
    c.aStarMove(hero,pX,pY)
  }
}

class Manhattan(c:ControllerInterface)extends TravelStrategy(c){
  override def movePossible(hero: model.Character, pX: Int, pY: Int): Boolean = {
    val xDistance = Math.abs(pX - 1 - hero.cell.x)
    val yDistance = Math.abs(pY - 1 - hero.cell.y)
    val distance =  xDistance + yDistance
    if(hero.mrange >= distance) true else throw new Exception("That's way too far away. I would never get there")
  }
}


