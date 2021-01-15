package XCOM.model

trait Scenario {
  def loadScenario(i: Int): Field
  def getAmmount():Int
}
