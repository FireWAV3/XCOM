package XCOM.model
import FieldStructure._

case class Character(name: String, mrange: Int, srange: Int, damage: Int, hp: Int, side: Int, displayname: String, cell: Cell) {

  def this() {
    this("Test", 2, 2, 0, 1, 0, "TT", Cell(0, 0, C))
  }

  override def toString: String = {
    ("The Character '" + this.name + "'(" + this.displayname + ", Team "
      + this.side + ") can move over " + this.mrange + " and shoot over " + this.srange
      + " tiles with a damage of " + this.damage + ". He has " + this.hp + " health points left.")
  }

}
