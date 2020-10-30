package XCOM

case class Field(pX: Int, pY: Int, cells: Vector[Cell], character: Vector[Character]) {
  var sizeX = pX - 1
  var sizeY = pY - 1


  override def toString: String = {
    var vectorcountF, vectorcountC = 0
    var count = 0
    var temp = "\t "
    var abc = "A"
    for (i <- 0 to sizeX) {
      temp += (abc + "\t")
      abc = (abc(0) + 1).toChar.toString
    }
    temp += "\n  "
    for (i <- 0 to sizeX) {
      temp += "----"
    }
    temp += "\n"
    for (i <- 0 to sizeY) {
      temp += (i + 1 + "\t|")
      for (j <- 0 to sizeX) {

        if (character.length > vectorcountC &&
          character(vectorcountC).cell.x == j &&
          character(vectorcountC).cell.y == i) {
          temp += this.character(vectorcountC).displayname
          temp += "\t"
          vectorcountC += 1

        } else if (cells.length > vectorcountF &&
          cells(vectorcountF).x == j &&
          cells(vectorcountF).y == i) {
          temp += this.cells(vectorcountF).otype
          temp += "\t"
          vectorcountF += 1
        } else {
          temp += "X\t"
        }
      }
      temp += "\n"
      count = count + 6
    }

    return temp;
  }
}
