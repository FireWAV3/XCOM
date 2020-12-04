package XCOM.controller
import XCOM.model.FieldStructure._
import XCOM.model.{AttackScenario, Cell, Character, Field, Scenario, TurnScenario}

trait GameStateTrait{
  //def handle(c : Controller,str : Vector[String], num :Vector[Int])
  def help()
  def exit(str:String)
  def info(str:String):Boolean
  def loadScenario(index:Int): Boolean
  def move(str:String, pX:Int, pY:Int): Boolean
  def aim(str1:String, str2:String): Boolean
  def shoot(approval:Boolean): Boolean
  def next(): Boolean
}

class Context(c : Controller){
  var state:GameStateTrait = new MenuState(c)
}

class GameState(c:Controller) extends GameStateTrait{
  override def help:Unit = {
    c.out("\nHELP" +
      "\nExit:\t\t\tExits the game" +
      "\nMove C,X,Y :\tMove Character(C) to X, Y" +
      "\nInfo C:\t\t\tCurrent status of Character(C)" +
      "\nShoot C,T:\t\tCharacter(C) attacks Target(T)\n")
  }

  override def exit(str:String): Unit = {
    c.out(str + "\nThanks for playing!\nGoodbye!\n")
    sys.exit()
  }

  override def info(str: String): Boolean = {
    val hero = c.isHero(str)
    if(hero._1){
      c.out(hero._2.toString)
      return true
    }
    c.out(str +" is not a Hero")
    false
  }

  override def loadScenario(index: Int): Boolean = {c.wrongGameState(); false}

  override def move(str: String, pX: Int, pY: Int): Boolean = {c.wrongGameState(); false}

  override def aim(str1: String, str2: String): Boolean = {c.wrongGameState(); false}

  override def shoot(approval: Boolean): Boolean = {c.wrongGameState(); false}

  override def next(): Boolean = {
    c.PlayerState = c.nextPlayerState(c.PlayerState)
    c.attack = new AttackScenario()
    c.turnS.load( PlayerStatus.turn(c.PlayerState),c.field)
    c.out("Turn of the " + c.PlayerState+" Team started")
    true
  }
}

class MenuState(c : Controller) extends GameState(c){

  override def info(str: String): Boolean ={c.wrongGameState(); false}

  override def next(): Boolean ={c.wrongGameState(); false}

  override def loadScenario(index: Int): Boolean = {
    val scenario = Scenario()
    c.field = scenario.loadScenario(index)
    c.out("Successfully loaded scenario "+ index +"\n"
      + "You can now enter"
      + "\nMove C,X,Y :\tMove Character(C) to X, Y"
      + "\nInfo C:\t\t\tCurrent status of Character(C)"
      + "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n")
    c.context.state = new SuiState(c)
    c.turnS.load(PlayerStatus.turn(c.PlayerState),c.field)
    true
  }

}

class SuiState(c : Controller) extends GameState(c) {

  override def move(str: String, pX: Int, pY: Int): Boolean = {
    val hero = c.isHero(str)
    if (hero._1) {
      if(PlayerStatus.turn(c.PlayerState) == hero._2.side ){
        if(c.turnS.movable(hero._2.displayname)){
          if (c.boundsX(pX) && c.boundsY(pY)) {
            if (!c.testHero(pX, pY) && !c.testRock(pX, pY)) {
              if (c.movePossible(hero._2, pX, pY)) {
                c.field = Field(c.field.pX, c.field.pY, c.field.rocks,
                  c.field.character.map { i =>
                    if (i.displayname == hero._2.displayname) {
                      c.turnS.movedHero(hero._2.displayname)
                      Character(i.name, i.mrange, i.srange, i.damage, i.hp, i.side, i.displayname, Cell(pX - 1, pY - 1, C))
                    } else {
                      i
                    }
                  }
                )
                c.out(" move successful")
                return true
              } else {
                c.out(" Move not possible: Hero can't move this far")
              }
            } else {
              c.out(" Move not possible: There is another object at this position")
            }
          } else {
            c.out(" Move not possible: not a tile on the field")
          }
        }else{
          c.out(str + " already moved")
        }
      }else {
        c.out(str + " is not a member of the Team that has the control")
      }
    } else {
      c.out(str + " is not a valid Hero")
    }
    false
  }

  override def aim(str1: String, str2: String): Boolean = {
    val hero1 = c.isHero(str1)
    val hero2 = c.isHero(str2)
    if(hero1._1 && hero2._1){
      if(PlayerStatus.turn(c.PlayerState) == hero1._2.side ){
        if(c.turnS.shootable(hero1._2.displayname)){
          if(hero1._2.side != hero2._2.side){
            val percentage = c.shootpercentage(hero1._2, hero2._2)
            c.attack = AttackScenario(hero1._2, hero2._2, percentage)
            c.out(("The chance to hit " + hero2._2.name + " (" + hero2._2.displayname+ ") with "
              + hero1._2.name + " (" + hero1._2.displayname + ") is: " + percentage
              + "%. If you want to shoot, enter 'Yes' otherwise enter 'No'"))
            c.context.state = new ShootState(c)
            return true
          }else{
            c.out("Heros are on the same team")
          }
        }else{
          c.out(str1 + " already shot")
        }
      }else {
        c.out(str1 + " is not a member of the Team that has the control")
      }
    }else{
      c.out("Please enter 2 valid heros")
    }
    false
  }
}

class ShootState(c : Controller) extends GameState(c){
  override def shoot(approval: Boolean): Boolean = {
    if(approval){
      c.turnS.shootHero(c.attack.attHero.displayname)
      val random = scala.util.Random
      val randInt = random.nextInt(101)
      if (randInt <= c.attack.probability){
        val result = c.fire(c.attack.attHero, c.attack.defHero)
        c.out((c.attack.attHero.name + " (" + c.attack.attHero.displayname + ") dealt "
          + result._1 + " damage to " + c.attack.defHero.name + " (" + c.attack.defHero.displayname
          + ")(" + result._2 + " hp left)"))
        c.context.state = new SuiState(c)
        c.attack = new AttackScenario()

        //Check defeated
        val turnSnext = TurnScenario()
        turnSnext.load( PlayerStatus.turn(c.nextPlayerState(c.PlayerState)),c.field)
        if(turnSnext.testEnd()){
          exit("The Team: " + c.PlayerState +" has won")
        }
        c.checkTurn()
        return true
      }else{
        c.out(("Shot missed by " + (randInt - c.attack.probability) + " cm"))
        c.context.state = new SuiState(c)
        c.attack = new AttackScenario()
      }
      c.checkTurn()
    }else{
      c.out("Shot canceled")
      c.context.state = new SuiState(c)
      c.attack = new AttackScenario()
    }
    false
  }
}




