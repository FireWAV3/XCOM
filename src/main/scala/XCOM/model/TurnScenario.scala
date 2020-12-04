package XCOM.model

case class TurnScenario() {

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
    if(shootable(hero)){
      return map(hero)
    }
    false
  }

  def shootable(hero: String) : Boolean = map.contains(hero)

  def movedHero(hero: String): Unit ={
    map += hero -> false
  }

  def shootHero(hero: String): Unit ={
    map -= hero
  }

  def testEnd(): Boolean = map.isEmpty
}