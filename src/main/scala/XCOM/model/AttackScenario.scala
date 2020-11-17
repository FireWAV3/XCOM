package XCOM.model

import XCOM.model

case class AttackScenario(attHero: model.Character, defHero: model.Character, probability: Int) {
  def this() {
    this(new model.Character(), new model.Character(), 0)
  }
}
