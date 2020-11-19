package XCOM.model


case class Field(pX: Int, pY: Int, rocks: Vector[Cell], character: Vector[Character]) {
  val sizeX = pX - 1
  val sizeY = pY - 1


  def this(pX: Int, pY: Int) {
    this(pX, pY, Vector[Cell](), Vector[Character]())
  }

  def this(character: Vector[Character]) {
    this(5, 10, Vector[Cell](), character)
  }

  def this(rocks: Vector[Cell], i: String) {
    this(5, 10, rocks, Vector[Character]())
  }

  override def toString: String = {
    if (this.sizeX < 0 || this.sizeY < 0) {
      return ""
    }
    var temp = "\t "
    temp += xAxisString(sizeX)
    for (i <- 0 to sizeY) {
      temp += (i + 1 + "\t|")
      temp += printRow(i)
      temp += "\n"
    }
    temp
  }

  def xAxisString(collums: Int): String = {
    var abc = "B"
    var tempReturn = "A\t"
    for (i <- 0 to collums - 1) {
      tempReturn += (abc + "\t")
      abc = (abc(0) + 1).toChar.toString
    }
    tempReturn += "\n  "
    for (i <- 0 to collums) {
      tempReturn += "----"
    }
    tempReturn += "\n"
    tempReturn
  }

  def fieldPosReturn(x: Int, y: Int): String = {
    character.foreach { p =>
      if (p.cell.x == x && p.cell.y == y) {
        if (p.side == 1) {
          return Console.RED + p.displayname + "\t" + Console.RESET
        } else {
          return Console.BLUE + p.displayname + "\t" + Console.RESET
        }

      }
    }
    rocks.foreach { r =>
      if (r.x == x && r.y == y) {
        return Console.WHITE_B + Console.BOLD + Console.BLACK + r.otype + Console.RESET + "\t"
      }
    }
    "X\t"
  }

  def printRow(yRow: Int): String = {
    var retString = ""
    for (j <- 0 to sizeX) {
      retString += fieldPosReturn(j, yRow)
    }
    retString
  }
}
