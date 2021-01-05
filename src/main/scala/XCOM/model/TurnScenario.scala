package XCOM.model
import scala.util.{Failure, Success, Try}

case class TurnScenario() {
  def deepCopy(): TurnScenario = {
    var TSout = new TurnScenario
    TSout.map = this.map
    TSout
  }


  var map = Map[String, Boolean]()


  def load(team : Int, field : Field): Unit ={
      map = Map[String, Boolean]()
      for(e <- field.character if e.side == team){
        map += (
            e.displayname -> true
        )
      }
  }

  def movable(hero: String) : Boolean ={
    Try(map(hero)) match {
      case Success(s) => if(s) s else throw new Exception("Man, I'm to exhausted to go anywhere right now")
      case Failure(exception) => throw new Exception("Need some time to reload first")
    }
  }

  def shootable(hero: String) : Boolean = {
    if (map.contains(hero)) true else throw new Exception("Need some time to reload first")
  }

  def movedHero(hero: String): Unit ={
    map += hero -> false
  }

  def shootHero(hero: String): Unit ={
    map -= hero
  }

  def testEnd(): Boolean = map.isEmpty
}