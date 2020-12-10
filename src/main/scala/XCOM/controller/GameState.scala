package XCOM.controller
import XCOM.model.FieldStructure._
import XCOM.model._

import scala.util.{Failure, Success, Try}

trait GameStateTrait{
  //def handle(c : Controller,str : Vector[String], num :Vector[Int])
  def help()
  def exit(str:String)
  def info(str:String):Boolean
  def loadScenario(index:Int): Boolean
  def move(str:String, pX:Int, pY:Int): Boolean
  def aim(str1:String, str2:String): Boolean
  def shoot(approval: Boolean,seed: Int): Boolean
  def next(): Boolean
}

class Context(c : Controller){

  def deepCoppy(): Context = {
    var Cout = new Context(this.c)
    Cout.state = this.state
    Cout
  }

  var state:GameStateTrait = new MenuState(c)
}

class GameState(c:Controller) extends GameStateTrait{
  override def help:Unit = {
    c.out("\nHELP" +
      "\nExit:\t\t\tExits the game" +
      "\nMove C,X,Y :\tMove Character(C) to X, Y" +
      "\nInfo C:\t\t\tCurrent status of Character(C)" +
      "\nShoot C,T:\t\tCharacter(C) attacks Target(T)\n"+
      "\nUndo:\t\tmove you back in Time to the last step you made\n"+
      "\nRedo:\t\treverts the Undo Time travel\n")
  }

  override def exit(str:String): Unit = {
    c.out(str + "\nThanks for playing!\nGoodbye!\n")
    sys.exit()
  }

  override def info(str: String): Boolean = {
    c.isHero(str) match {
      case Some(value) => c.out(value.toString) ; true
      case None =>  c.out(str +" is not a Hero") ; false
    }
  }

  override def loadScenario(index: Int): Boolean = {c.wrongGameState(); false}

  override def move(str: String, pX: Int, pY: Int): Boolean = {c.wrongGameState(); false}

  override def aim(str1: String, str2: String): Boolean = {c.wrongGameState(); false}

  override def shoot(approval: Boolean,seed: Int): Boolean = {c.wrongGameState(); false}

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
    hero match {
      case Some(value) => {
        val allowed = Try(
              c.checkSide(value.side)
          && c.turnS.movable(value.displayname)
          && c.boundsX(pX)
          && c.boundsY(pY)
          && c.testHero(pX, pY)
          && c.testRock(pX, pY)
          && c.movePossible(value, pX, pY)
        )
        allowed match {
          case Success(bool) => {
            c.field = Field(c.field.pX, c.field.pY, c.field.rocks,
              c.field.character.map { i =>
                i.displayname match {
                  case value.displayname =>{
                    c.turnS.movedHero(value.displayname)
                    Character(i.name, i.mrange, i.srange, i.damage, i.hp, i.side, i.displayname, Cell(pX - 1, pY - 1, C))
                  }
                  case _ => i
                }
              }
            )
            c.out(" move successful")
            true
          }
          case Failure(exception) => c.out(exception.getMessage); false
        }
      }
      case None =>  c.out(str + " is not a valid Hero") ; false
    }
  }

  override def aim(str1: String, str2: String): Boolean = {
    val hero1 = c.isHero(str1)
    val hero2 = c.isHero(str2)
    hero1 match {
      case Some(valueH1) =>{
        hero2 match {
          case Some(valueH2) =>{
            val allowed = Try(
                 c.checkSide(valueH1.side)
              && c.turnS.shootable(valueH1.displayname)
              && c.opponent(valueH1,valueH2)
            )
            allowed match {
              case Success(bool) =>{
                val percentage = c.shootpercentage(valueH1, valueH2)
                c.attack = AttackScenario(valueH1, valueH2, percentage)
                c.seed = scala.util.Random.nextInt()
                c.out(("The chance to hit " + valueH2.name + " (" + valueH2.displayname+ ") with "
                  + valueH1.name + " (" + valueH1.displayname + ") is: " + percentage
                  + "%. If you want to shoot, enter 'Yes' otherwise enter 'No'"))
                c.context.state = new ShootState(c)
                true
              }
              case Failure(exception) => c.out(exception.getMessage); false
            }
          }
          case None =>  c.out(str2 + " is not a valid Hero"); false
        }
      }
      case None => c.out(str1 + " is not a valid Hero"); false
    }
  }
}

class ShootState(c : Controller) extends GameState(c){
  override def shoot(approval: Boolean,seed: Int): Boolean = {
    if(approval){
      c.turnS.shootHero(c.attack.attHero.displayname)
      val random = new scala.util.Random(seed)
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




