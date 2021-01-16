package XCOM.model
import FieldStructure._
//Used to store all informations for a Character
case class Character(name: String, mrange: Int, srange: Int, damage: Int, hp: Int, side: Int, displayname: String, cell: Cell) {

  def this() {
    this("Test", 2, 2, 0, 1, 0, "TT", Cell(0, 0, C))
  }

  override def toString: String = {
    ("Classname:     " + this.name + "\nName:          " + this.displayname + "\nTeam:          "
      + this.side + "\nMovementrange: " + this.mrange + "\nShootingrange: " + this.srange
      + "\nDamage:        " + this.damage + "\nHP:            " + this.hp)
  }
}
