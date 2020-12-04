package XCOM.controller
import XCOM.controller.PlayerStatus._
import XCOM.model
import XCOM.model.{AttackScenario, Character, Field, Scenario, TurnScenario}
import XCOM.util.Observable



case class Controller(var field: Field, var attack : AttackScenario) extends Observable{

  var context = new Context(this)
  var contextTravel = new ContextTravel(this)
  var output = ""

  //TODO in Kunstruktor
  var PlayerState : PlayerStatus = BLUE
  val turnS = TurnScenario()

  def this (){
    this(new Field(0,0),new AttackScenario())
  }

  def help:Unit={
    context.state.help()
  }

  def exit:Unit ={
    context.state.exit("")
  }

  def info(str: String): Boolean ={
    context.state.info(str)
  }

  def loadScenario(index:Int): Boolean ={
    context.state.loadScenario(index)
  }

  def move(str: String,pX:Int, pY:Int):Boolean ={
    context.state.move(str, pX, pY)
  }

  def aim(str1:String, str2:String):Boolean ={
    context.state.aim(str1, str2)
  }

  def shoot(approval:Boolean):Boolean={
    context.state.shoot(approval)
  }

  def next() : Boolean = {
    context.state.next()
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
      out("Turn of the " + PlayerState+" Team started")
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
    if(field.sizeX >= x-1 && x-1 >= 0){
      return true
    }
    false
  }

  def boundsY(y:Int): Boolean ={
    if(field.sizeY >= y-1 && y-1 >= 0){
      return true
    }
    false
  }

  def testRock( pX: Int, pY: Int): Boolean = {
    for(e <- field.rocks if e.x == (pX-1)  if e.y == (pY-1) ) return true
    false
  }

  def testHero(pX: Int, pY: Int): Boolean = {
    for(e <- field.character if e.cell.x == pX-1 && e.cell.y == pY-1)return true
    false
  }

  def isHero(input : String): (Boolean,Character)= {
    field.character.map(i => if(i.displayname == input) return (true,i))
    (false,new Character())
  }

  def aStarMove(startX:Int,startY:Int,goalX:Int,goalY:Int) : Boolean = {
    //TODO A*
    out("not implemented yet")
    false
  }

  def movePossible(hero:model.Character, pX:Int, pY:Int):Boolean = {
    if(true/*hero.ability >= 10*/){//TODO implement ability
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
    notifyObservers
  }



  def wrongInput(input : String):Unit={
    out("Wrong input: [" + input +"]")
  }

  def wrongGameState() = out("You are not allowed to use that command right now")

  def fieldToString:String = field.toString

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

}
