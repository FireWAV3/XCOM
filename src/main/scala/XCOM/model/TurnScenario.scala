package XCOM.model
import scala.util.{Failure, Success, Try}

//keeps Track which Characters already moved or shot in this turn
case class TurnScenario() {
  def deepCopy(): TurnScenario = {
    val TSout = new TurnScenario
    TSout.map = this.map
    TSout
  }


  var map = Map[String, Boolean]()

  //initialize this turn
  def load(team : Int, field : Field): Unit ={
      map = Map[String, Boolean]()
      for(e <- field.character if e.side == team){
        map += (
            e.displayname -> true
        )
      }
  }

  //check if Character already moved or shot
  def movable(hero: String) : Boolean ={
    Try(map(hero)) match {
      case Success(s) => if(s) s else throw new Exception("Man, I'm to exhausted to go anywhere right now")
      case Failure(exception) => throw new Exception("Need some time to reload first")
    }
  }

  //check if Character already shot
  def shootable(hero: String) : Boolean = {
    if (map.contains(hero)) true else throw new Exception("Need some time to reload first")
  }

  //save that Character moved
  def movedHero(hero: String): Unit ={
    map += hero -> false
  }

  //save that Character shot
  def shootHero(hero: String): Unit ={
    map -= hero
  }

  //Check if all Characters shot this turn
  def testEnd(): Boolean = map.isEmpty
}