package XCOM

case class AttackScenario(attHero:Character, defHero:Character, probability:Int) {
    def this(){
      this(new Character(), new Character(), 0)
    }
}
