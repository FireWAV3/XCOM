package XCOM.controller
import XCOM.controller.GameStatus._
import XCOM.model
import XCOM.model.FieldStructure._
import XCOM.model.{AttackScenario, Cell, Character, Field, Scenario, TurnScenario}
import XCOM.util.Observable
import XCOM.controller.PlayerStatus._



case class Controller(var gameState: GameState,var field: Field, var attack : AttackScenario) extends Observable{


  var output = ""

  //TODO in Kunstruktor
  var PlayerState : PlayerStatus = BLUE
  val turnS = TurnScenario()

  def this (){
    this(MENU, new Field(0,0),new AttackScenario())
  }

  def this (gameState:GameState){
    this(gameState, new Field(0,0),new AttackScenario())
  }

  def help:Unit={
    singleOut("help",HELP)
  }

  def exit:Unit ={
    gameState = END
    notifyObservers
  }

  def info(str: String): Boolean ={
    val hero = isHero(str)
    if(hero._1){
      singleOut(hero._2.toString,SINGLEOUT)
      return true
    }
    singleOut(str +" is not a Hero",SINGLEOUT)
    false
  }


  def loadScenario(index:Int): Boolean ={
    val scenario = Scenario()
    if(gameState == MENU){
      field = scenario.loadScenario(index)
      out("Successfully loaded scenario "+ index +"\n"
        + "You can now enter"
        + "\nMove C,X,Y :\tMove Character(C) to X, Y"
        + "\nInfo C:\t\t\tCurrent status of Character(C)"
        + "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n"
        ,SUI)
      turnS.laod(PlayerStatus.turn(PlayerState),field)
      return true
    }
    false
  }


  def move(str: String,pX:Int, pY:Int):Boolean ={
    val hero = isHero(str)
    if(hero._1){
      if(PlayerStatus.turn(PlayerState) == hero._2.side ){
        if(turnS.movable(hero._2.displayname)){
          return moveI(str, pX, pY)
        }else{
          singleOut(str + " already moved", SINGLEOUT)
        }
      }else {
        singleOut(str + " is not a member of the Team that has the control", SINGLEOUT)
      }
    }else{
      singleOut(str + " is not a valid Hero", SINGLEOUT)
    }
    false
  }


  def moveI(str: String,pX:Int, pY:Int):Boolean ={
    if(gameState == SUI) {
      val hero = isHero(str)
      if (hero._1) {
        if (boundsX(pX) && boundsY(pY)) {
          if (!testHero(pX, pY) && !testRock(pX, pY)) {
            if (movePossible(hero._2, pX, pY)) {
              field = Field(field.pX, field.pY, field.rocks,
                field.character.map { i =>
                  if (i.displayname == hero._2.displayname) {
                    turnS.movedHero(hero._2.displayname)
                    Character(i.name, i.mrange, i.srange, i.damage, i.hp, i.side, i.displayname, Cell(pX - 1, pY - 1, C))
                  } else {
                    i
                  }
                }
              )
              out(" move successful", SUI)
              return true
            } else {
              singleOut(" Move not possible: Hero can't move this far", SINGLEOUT)
            }
          } else {
            singleOut(" Move not possible: There is another object at this position", SINGLEOUT)
          }
        } else {
          singleOut(" Move not possible: not a tile on the field", SINGLEOUT)
        }
      } else {
        singleOut(str + " is not a valid Hero", SINGLEOUT)
      }
    } else {
      wrongGameState()
    }
    false
  }

  def aim(str1:String, str2:String):Boolean ={
    val hero1 = isHero(str1)
    val hero2 = isHero(str2)
    if(hero1._1 && hero2._1){
      if(PlayerStatus.turn(PlayerState) == hero1._2.side ){
        if(turnS.shootable(hero1._2.displayname)){

          return aimI(str1:String, str2:String)
        }else{
          singleOut(str1 + " already shot", SINGLEOUT)
        }
      }else {
        singleOut(str1 + " is not a member of the Team that has the control", SINGLEOUT)
      }
    }else{
      singleOut(" Please enter 2 valid heros",SINGLEOUT)
    }
    false
  }

  def aimI(str1:String, str2:String):Boolean ={
    if(gameState == SUI){
      val hero1 = isHero(str1)
      val hero2 = isHero(str2)
      if(hero1._1 && hero2._1){
        if(hero1._2.side != hero2._2.side){
          val percentage = shootpercentage(hero1._2, hero2._2)
          attack = AttackScenario(hero1._2, hero2._2, percentage)
          out(("The chance to hit " + hero2._2.name + " (" + hero2._2.displayname+ ") with "
            + hero1._2.name + " (" + hero1._2.displayname + ") is: " + percentage
            + "%. If you want to shoot, enter 'Yes' otherwise enter 'No'")
            ,SHOOT)
          return true
        }else{
          singleOut("Heros are on the same team",SINGLEOUT)
        }
      }else{
        singleOut("Please enter 2 valid heros",SINGLEOUT)
      }
    }else{
      wrongGameState()
    }
    false
  }

  //TODO ander pos im code
  def nextPlayerState(side: PlayerStatus) : PlayerStatus = {
    if(PlayerState == BLUE){
      return RED
    }
    BLUE
  }

  def next : Boolean = {
    if(gameState == SUI){
      PlayerState = nextPlayerState(PlayerState);
      turnS.laod( PlayerStatus.turn(PlayerState),field)
      singleOut("Turn of the " + PlayerState+" Team started" ,SUI)
      return true
    }else{
      wrongGameState()
    }
    false
  }

  def checkTurn() : Boolean = {
    if(turnS.testEnd()){
      PlayerState = nextPlayerState(PlayerState)
      turnS.laod( PlayerStatus.turn(PlayerState),field)
      singleOut("Turn of the " + PlayerState+" Team started" ,SINGLEOUT)
      return true
    }
    false
  }

  def shoot(approval:Boolean):Boolean={
    if(gameState == SHOOT){
      if(approval){
        turnS.shootHero(attack.attHero.displayname)
        val random = scala.util.Random
        val randInt = random.nextInt(101)
        if (randInt <= attack.probability){
          val result = fire(attack.attHero, attack.defHero)
          out((attack.attHero.name + " (" + attack.attHero.displayname + ") dealt "
            + result._1 + " damage to " + attack.defHero.name + " (" + attack.defHero.displayname
            + ")(" + result._2 + " hp left)")
            ,SUI)

          attack = new AttackScenario()


          //Check defeated
          val turnSnext = TurnScenario()
          turnSnext.laod( PlayerStatus.turn(nextPlayerState(PlayerState)),field)
          if(turnSnext.testEnd()){
            out("The Team: " + nextPlayerState(PlayerState) +" has won",END)
          }
          checkTurn()
          return true
        }else{
          out(("Shot missed by " + (randInt-attack.probability) + " cm"),SUI)
          attack = new AttackScenario()
        }
        checkTurn()
      }else{
        out("Shot canceled",SUI)
        attack = new AttackScenario()
      }
    }else{
      wrongGameState()
    }
    false
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


  def movePossible(hero:model.Character, pX:Int, pY:Int):Boolean = {
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

  def shootpercentage(attHero: model.Character, defHero: model.Character): Int ={
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

  def out(str:String,state :GameState):Unit ={
    output = str
    gameState = state
    notifyObservers
  }

  def singleOut(message : String,state : GameState){
    val oldState = gameState
    gameState = state
    output = message
    notifyObservers
    gameState = oldState
  }


  def wrongInput(input : String):Unit={
    singleOut("Wrong input: [" + input +"]",SINGLEOUT)
  }

  def wrongGameState() = singleOut("You are not allowed to use that command right now",SINGLEOUT)

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
