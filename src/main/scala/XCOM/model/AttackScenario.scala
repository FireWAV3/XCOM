package XCOM.model

//Stores Information to get Transition from SUI to Shoot-State
case class AttackScenario(attHero: Character, defHero: Character, probability: Int) {
   def this() {
    this(new Character(), new Character(), 0)
  }
}
