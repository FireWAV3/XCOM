package XCOM.model
import XCOM.model.FieldStructure._

case class Scenario() {
  val amount = 2
  //TODO laod Scenario form .xml File

  def loadScenario(i: Int): Field = {
    //TODO: create different scenarios

    i match {
      case 0 => {
        val sniper = Character("Sniper", 5, 10, 70, 40, 0, "C1", Cell(5, 1, C))
        val tank = Character("Tank", 5, 10, 30, 40, 0, "C2", Cell(4, 4, C))
        val assassin = Character("Assassin", 8, 1, 150, 50, 1, "C3", Cell(6, 4, C))
        val assassinT = Character("Assassin Nr.2", 8, 1, 150, 10, 1, "C4", Cell(5, 5, C))
        val allCharacter = Vector[Character](sniper, tank, assassin,assassinT)
        val field = Field(6 + 1, 20 + 1, Vector[Cell](
          Cell(5, 0, R), Cell(2, 2, R), Cell(3, 2, R), Cell(3, 6, R), Cell(3, 7, R)),
          allCharacter)

        field
      }
      case 1 => {
        val sniper = Character("Sniper", 5, 10, 70, 40, 0, "C1", Cell(5, 1, C))
        val tank = Character("Tank", 5, 10, 30, 40, 0, "C2", Cell(4, 4, C))
        val assassin = Character("Assassin", 8, 1, 150, 50, 1, "C3", Cell(6, 4, C))
        val assassinT = Character("Assassin Nr.2", 8, 1, 150, 10, 1, "C4", Cell(5, 5, C))
        val allCharacter = Vector[Character](sniper, tank, assassin,assassinT)
        val field = Field(6 + 1, 10 + 1, Vector[Cell](
          Cell(5, 0, R), Cell(2, 2, R), Cell(3, 2, R), Cell(3, 6, R), Cell(3, 7, R)),
          allCharacter)

        field
      }
      case 2 => {
        val sniper = Character("Sniper", 5, 10, 70, 40, 0, "C1", Cell(5, 1, C))
        val tank = Character("Tank", 5, 10, 30, 40, 0, "C2", Cell(4, 4, C))
        val tank2 = Character("Tank", 5, 10, 30, 40, 0, "C3", Cell(5, 4, C))
        val tank3 = Character("Tank", 5, 10, 30, 40, 0, "C4", Cell(3, 4, C))
        val assassin = Character("Assassin", 8, 1, 150, 50, 1, "C5", Cell(1, 18, C))
        val assassinT = Character("Assassin Nr.2", 8, 1, 150, 10, 1, "C6", Cell(3, 9, C))
        val assassinTa = Character("Assassin Nr.3", 8, 1, 150, 10, 1, "C7", Cell(5, 10, C))
        val assassinTb = Character("Assassin Nr.4", 8, 1, 150, 10, 1, "C8", Cell(4, 20, C))
        val allCharacter = Vector[Character](sniper, tank,tank2,tank3, assassin,assassinT,assassinTa,assassinTb)
        val field = Field(8 + 1, 20 + 1, Vector[Cell](
          Cell(5, 0, R), Cell(2, 2, R), Cell(3, 2, R), Cell(3, 6, R), Cell(3, 7, R)),
          allCharacter)

        field
      }
      case _ => new Field(1, 1)
    }
  }
}
