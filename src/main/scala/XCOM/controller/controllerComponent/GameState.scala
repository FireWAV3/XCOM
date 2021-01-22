package XCOM.controller.controllerComponent

import XCOM.XcomModule
import XCOM.model.FieldStructure._
import XCOM.model._
import com.google.inject.{Guice, Inject}
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector

import scala.util.{Failure, Success, Try}

//State Pattern for Gamestates
trait GameStateTrait{
  def help()
  def exit(str:String)
  def info(str:String):Boolean
  def loadScenario(index:Int): Boolean
  def move(str:String, pX:Int, pY:Int): Boolean
  def aim(str1:String, str2:String): Boolean
  def shoot(approval: Boolean,seed: Int): Boolean
  def next(): Boolean
}

//keep track of the current state
class Context(c : ControllerInterface){

  def deepCopy(): Context = {
    val Cout = new Context(this.c)
    Cout.state = this.state
    Cout
  }

  var state:GameStateTrait = new MenuState(c)
}

//default behavior of the states
class GameState(c:ControllerInterface) extends GameStateTrait{
  override def help:Unit = {
    c.helpOut
  }

  override def exit(str:String): Unit = {
    c.out(str + "\nThanks for playing!\nGoodbye!\n")
    throw new Exception("EXIT")
  }

  override def info(str: String): Boolean = {
    c.isHero(str) match {
      case Some(value) => c.output = value.toString.replaceAll("Team:          0",
        "Team: Blue").replaceAll("Team:          1", "Team: Red"); c.infoOut; true
      case None =>  c.out("Who do you mean? We don't know " + str) ; false
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
    c.output = "Let's go " + c.PlayerState +". Time to kick some ass!"
    c.publish(new UpdateField)
    true
  }
}

//Initial State to choose a Scenario to play
class MenuState @Inject() (c : ControllerInterface) extends GameState(c){

  val injector = Guice.createInjector(new XcomModule)

  //block methods which are avaiable in every state but this
  override def info(str: String): Boolean ={c.wrongGameState(); false}

  override def next(): Boolean ={c.wrongGameState(); false}

  override def loadScenario(index: Int): Boolean = {
    val scenario = injector.instance[Scenario]
    c.field = scenario.loadScenario(index)
    c.output = ("Successfully loaded scenario "+ index +"\n")
    c.publish(new UpdateMenu)
    c.context.state = new SuiState(c)
    c.turnS.load(PlayerStatus.turn(c.PlayerState),c.field)
    true
  }

}

//default State while Playing where you can do almost everything
class SuiState(c : ControllerInterface) extends GameState(c) {

  //Moving a Character to a given position
  override def move(str: String, pX: Int, pY: Int): Boolean = {
    val hero = c.isHero(str)
    hero match {//check if Hero exists
      case Some(value) => {
        val allowed = Try(//check all illegal movements
              c.checkSide(value.side)
          && c.turnS.movable(value.displayname)
          && c.boundsX(pX)
          && c.boundsY(pY)
          && c.testHero(pX, pY)
          && c.testRock(pX, pY)
          && c.movePossible(value, pX, pY)
        )
        allowed match {
          case Success(bool) => {//move Character to his new position
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
            c.output = "Finally got there. This was exhausting"
            c.publish(new UpdateField)
            true
          }
          case Failure(exception) => c.out(exception.getMessage); false
        }
      }
      case None =>  c.out("Who do you mean? We don't know " + str) ; false
    }
  }

  //Set up a shot by checking for illegal shots and calculating shootingpercentage
  override def aim(str1: String, str2: String): Boolean = {
    val hero1 = c.isHero(str1)
    val hero2 = c.isHero(str2)
    hero1 match {
      case Some(valueH1) =>{
        hero2 match {//check if both Characters exist
          case Some(valueH2) =>{
            val allowed = Try(//check for illegal shots
                 c.checkSide(valueH1.side)
              && c.turnS.shootable(valueH1.displayname)
              && c.opponent(valueH1,valueH2)
            )
            allowed match {
              case Success(bool) =>{//calculate shootingpercentage and generate seed to prevent Undo-cheating
                val percentage = c.shootpercentage(valueH1, valueH2)
                c.attack = AttackScenario(valueH1, valueH2, percentage)
                c.seed = scala.util.Random.nextInt()
                c.output =("Guess I can hit " + valueH2.displayname + " with a chance of like " + percentage + "%.")
                c.context.state = new ShootState(c)
                c.publish(new UpdateShoot)
                true
              }
              case Failure(exception) => c.out(exception.getMessage); false
            }
          }
          case None =>  c.out("Who do you mean? I don't know " + str2); false
        }
      }
      case None => c.out("Who do you mean? We don't know " + str1); false
    }
  }
}

//State in which the player is asked whether he wants to shoot with given percentage
class ShootState(c : ControllerInterface) extends GameState(c){
  override def shoot(approval: Boolean,seed: Int): Boolean = {
    if(approval){//Try to hit the other Character with given percentage
      c.turnS.shootHero(c.attack.attHero.displayname)
      val random = new scala.util.Random(seed)
      val randInt = random.nextInt(101)
      if (randInt <= c.attack.probability){
        val result = c.fire(c.attack.attHero, c.attack.defHero)
        c.output = ("Yes, got him. I took about " + result._1 + " hp, so he should have "
          + result._2 + " hp left. What a great shot!")
        c.publish(new UpdateField)
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
        c.out("Damnit I missed him. Would have taken only " + (randInt - c.attack.probability) + " cm")
        c.context.state = new SuiState(c)
        c.attack = new AttackScenario()
      }
      c.checkTurn()
    }else{//Shot canceled by user
      c.out("Affirmative. Target unlocked, safety back on")
      c.context.state = new SuiState(c)
      c.attack = new AttackScenario()
    }
    false
  }
}




