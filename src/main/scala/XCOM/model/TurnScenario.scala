package XCOM.model

case class TurnScenario() {
  def deepCoppy(): TurnScenario = {
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
    //map(hero) match { case true => true ; case false => throw new Exception("Already moved or shot with this Character")}
    if(map(hero)) true else throw new Exception("Already moved or shot with this Character")
  }

  def shootable(hero: String) : Boolean = {
    if (map.contains(hero)) true else throw new Exception("Already shot with this Character")
  }

  def movedHero(hero: String): Unit ={
    map += hero -> false
  }

  def shootHero(hero: String): Unit ={
    map -= hero
  }

  def testEnd(): Boolean = map.isEmpty
}