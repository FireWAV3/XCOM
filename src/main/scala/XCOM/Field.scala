package XCOM

case class Field(pX: Int, pY: Int, cells: Vector[Cell], character: Vector[Character]) {
  var sizeX = pX - 1
  var sizeY = pY - 1


  override def toString: String = {
    var vectorcountF, vectorcountC = 0
    var temp = "\t "
    temp += xAxisString(sizeX)
    for (i <- 0 to sizeY) {
      temp += (i + 1 + "\t|")
      temp += printRow(sizeX, i, this.cells, character)
      temp += "\n"
    }

    return temp;
  }
  def xAxisString(collums: Int) : String = {
    var abc = "B"
    var tempReturn = "A\t"
    for(i <- 0 to collums-1) {
      tempReturn += (abc+ "\t")
      abc = (abc(0) + 1).toChar.toString
    }
    tempReturn += "\n  "
    for (i <- 0 to collums){
      tempReturn += "----"
    }
    tempReturn += "\n"
    tempReturn
  }

  def fieldPosReturn(x:Int, y:Int, rocks:Vector[Cell], player:Vector[Character]) :String = {
    player.foreach{ p =>
      if(p.cell.x == x && p.cell.y == y){
        return Console.BLUE + p.displayname + "\t" + Console.RESET
      }
    }

    rocks.foreach{ r =>
      if(r.x == x && r.y == y){
        return Console.WHITE_B + Console.BLACK + r.otype + Console.RESET + "\t"//If otype is Enum, use toString of otype
      }
    }
    return "X\t"
  }

  def printRow (xLength:Int, yRow:Int, rocks:Vector[Cell], player:Vector[Character]) :String = {
    var retString = ""
    for (j <- 0 to xLength){
      retString += fieldPosReturn(j, yRow, rocks, player)
    }
    retString
  }
}

