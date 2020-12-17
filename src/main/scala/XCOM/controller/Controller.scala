package XCOM.controller
import XCOM.model
import XCOM.model.PlayerStatus._
import XCOM.model.{AttackScenario, Character, Field, PlayerStatus, Scenario, TurnScenario}
import XCOM.util.UndoManager

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.swing.Publisher
import scala.util.Try


case class Controller(var field: Field, var attack : AttackScenario) extends Publisher{

  var context = new Context(this)
  var contextTravel = new ContextTravel(this)
  var output = ""
  var seed = 0
  var PlayerState : PlayerStatus = BLUE
  var turnS = TurnScenario()

  def this (){
    this(new Field(0,0),new AttackScenario())
  }

  def deepCopy():Controller = {
    var Cout = Controller(this.field,this.attack)
    Cout.context = this.context.deepCoppy()
    Cout.contextTravel=  this.contextTravel.deepCoppy()
    Cout.turnS = this.turnS.deepCoppy()
    Cout.output =  this.output
    Cout.seed =  this.seed
    Cout.PlayerState = this.PlayerState
    Cout
  }

  def help:Unit={
    context.state.help()
  }

  def exit:Unit ={
    context.state.exit("")
  }

  def info(str: Option[String]): Boolean ={
    str match {
      case Some(s) => {
        context.state.info(s)
      }
      case None => false
    }
  }

  def loadScenario(index:Int): Boolean ={
    context.state.loadScenario(index)
  }

  def move(str: String,pX:Int, pY:Int):Boolean ={
    context.state.move(str, pX, pY)
  }

  def aim(str1: Option[String], str2: Option[String]):Boolean ={
    str2 match {
      case Some(s) => {
        context.state.aim(str1.get, str2.get)
        true
      }
      case None => false
    }
  }

  def shoot(approval:Boolean):Boolean={
    context.state.shoot(approval,seed)
  }

  def next() : Boolean = {
    context.state.next()
  }

  def undo(uManager: UndoManager) = {
    val newC = uManager.undoStep(this)
    field = newC.field
    attack = newC.attack
    turnS = newC.turnS
    context = newC.context
    contextTravel=  newC.contextTravel
    output = "step was undone \n "
    seed =  newC.seed
    PlayerState = newC.PlayerState
    publish(new UpdateField)
  }
  def redo(uManager: UndoManager) = {
    val newC = uManager.redoStep(this)
    field = newC.field
    attack = newC.attack
    turnS = newC.turnS
    context = newC.context
    contextTravel=  newC.contextTravel
    output = "step was redone \n "
    seed =  newC.seed
    PlayerState = newC.PlayerState
    publish(new UpdateField)
  }

  def fire(attHero: model.Character, defHero: model.Character): (Int,Int) ={
    if(defHero.hp - attHero.damage <= 0){
      val temp = field.character.filter(i => i.displayname!= defHero.displayname)
      field = Field(field.pX,field.pY,field.rocks,temp)
    }else{
      val temp = field.character.map{ i =>
        if(i.displayname == defHero.displayname ){
          Character(i.name,i.mrange,i.srange,i.damage,i.hp - attHero.damage,i.side,i.displayname,i.cell)
        }else{
          i
        }
      }
      field = Field(field.pX,field.pY,field.rocks,temp)
    }

    (attHero.damage, if ((defHero.hp-attHero.damage)>0) (defHero.hp-attHero.damage) else 0)
  }

  def checkTurn() : Boolean = {
    if(turnS.testEnd()){
      PlayerState = nextPlayerState(PlayerState)
      turnS.load( PlayerStatus.turn(PlayerState),field)
      output = "Turn of the " + PlayerState+" Team started"
      publish(new UpdateField)
      return true
    }
    false
  }

  def nextPlayerState(side: PlayerStatus) : PlayerStatus = {
    if(PlayerState == BLUE){
      return RED
    }
    BLUE
  }

  def boundsX(x:Int): Boolean ={
    if(field.sizeX >= x-1 && x-1 >= 0)true else throw new Exception("Move not possible: not a tile on the field")
  }

  def boundsY(y:Int): Boolean ={
    if(field.sizeY >= y-1 && y-1 >= 0) true else throw new Exception("Move not possible: not a tile on the field")
  }

  def testRock( pX: Int, pY: Int): Boolean = {
    for(e <- field.rocks if e.x == (pX-1)  if e.y == (pY-1) )  throw new Exception("Move not possible: There is a Rock at this position")
    true
  }

  def testHero(pX: Int, pY: Int): Boolean = {
    for(e <- field.character if e.cell.x == pX-1 && e.cell.y == pY-1) throw new Exception("Move not possible: There is another Hero at this position")
    true
  }

  def getHero(pX: Int, pY: Int): Character = {
    for(e <- field.character if e.cell.x == pX-1 && e.cell.y == pY-1) return e
    throw new Exception()
  }

  def isHero(input : String): Option[Character]= {
    field.character.map(i => if(i.displayname == input) return Some(i))
    None
  }

  def aStarMove(startX:Int,startY:Int,goalX:Int,goalY:Int) : Boolean = {
    //TODO A*
    throw new Exception("not implemented yet")
  }

  def movePossible(hero:model.Character, pX:Int, pY:Int):Boolean = {
    if(hero.side >= 0/*hero.ability >= 10*/){//TODO implement ability
      contextTravel.travelState = new Manhattan(this)
    }else{
      contextTravel.travelState = new AStar(this)
    }
    contextTravel.travelState.movePossible(hero, pX, pY)
  }

  def shootpercentage(attHero: model.Character, defHero: model.Character): Int ={
    val xDistance = Math.abs(attHero.cell.x - defHero.cell.x)
    val yDistance = Math.abs(attHero.cell.y - defHero.cell.y)
    val distance = xDistance + yDistance
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

  def out(str:String):Unit ={
    output = str
    publish(new UpdateText)
  }



  def wrongInput(input : String):Unit={
    out("Wrong input: [" + input +"]")
  }

  def wrongGameState() = out("You are not allowed to use that command right now")

  def fieldToString:String = field.toString

  def getCharacters: Vector[(String,Int,Int)] = {
    var temp = new ListBuffer[(String,Int,Int)]()
    for (x <- field.character) {temp.append((x.displayname,x.cell.x,x.cell.y))}
    temp.toVector
  }

  def getRocks: Vector[(Int,Int)] = {
    var temp = new ListBuffer[(Int,Int)]()
    for (x <- field.rocks) {temp.append((x.x,x.y))}
    temp.toVector
  }

  def scenarioAmmount:Int ={
    val scenario = Scenario()
    scenario.amount
  }

  def splitFlatString(input:String):Array[String] = {
    input.replace(',',' ').split("\\s+")
  }

  def testInt(input:String):Boolean = {
    input.forall(_.isDigit)
  }

  def abcToInt(str: String):Int = {
    val chr = str.charAt(0)
    chr - 'A' +1
  }

  def testABC(str: String):Boolean = {
    if(str.length == 1){
      val chr = str.charAt(0)
      if(chr >= 'A' && chr <= 'A'+ field.sizeX){
        return true
      }
    }
    false
  }

  def opponent(hero1: Character, hero2: Character): Boolean = {
    if(hero1.side == hero2.side) throw new Exception("Heros are on the same team") else true
  }

  def scenarioAmmountTest(input: Try[Int]):Try[Boolean] = Try(input.get >= 0 && input.get <= scenarioAmmount)

  def checkSide(side: Int): Boolean = {
    if(PlayerStatus.turn(PlayerState) == side) true else throw new Exception("Not a member of the Team that has the control")
  }

  def helpOut = publish(new UpdateHelp)

  def infoOut = publish(new UpdateInfo)
}
