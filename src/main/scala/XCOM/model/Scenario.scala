package XCOM.model
import XCOM.model.FieldStructure._

case class Scenario() {
  val amount = 1
  //TODO laod Scenario form .xml File

  def loadScenario(i: Int): Field = {
    //TODO: create different scenarios

    i match {
      case 0 => {
        val sniper = Character("Sniper", 5, 10, 70, 40, 0, "C1", Cell(5, 1, C))
        val tank = Character("Tank", 5, 10, 30, 40, 0, "C2", Cell(4, 4, C))
        val assassin = Character("Assassin", 8, 1, 150, 50, 1, "C3", Cell(6, 4, C))
        val allCharacter = Vector[Character](sniper, tank, assassin)
        val field = Field(6 + 1, 20 + 1, Vector[Cell](
          Cell(5, 0, R), Cell(2, 2, R), Cell(3, 2, R), Cell(3, 6, R), Cell(3, 7, R)),
          allCharacter)

        field
      }
      case 1 => {
        val sniper = Character("Sniper", 5, 10, 70, 40, 0, "C1", Cell(5, 1, C))
        val tank = Character("Tank", 5, 10, 30, 40, 0, "C2", Cell(4, 4, C))
        val assassin = Character("Assassin", 8, 1, 150, 50, 1, "C3", Cell(6, 4, C))
        val allCharacter = Vector[Character](sniper, tank, assassin)
        val field = Field(6 + 1, 20 + 1, Vector[Cell](
          Cell(5, 0, R), Cell(2, 2, R), Cell(3, 2, R), Cell(3, 6, R), Cell(3, 7, R)),
          allCharacter)

        field
      }
      case _ => new Field(1, 1)
    }
  }
}
