package XCOM

case class Field(pX: Int, pY: Int, cells: Vector[Cell], character: Vector[Character]) {
  var sizeX = pX - 1
  var sizeY = pY - 1


  override def toString: String = {
    var vectorcountF, vectorcountC = 0
    var temp = "\t "
    /*var abc = "A" //ersetzt durch xAxisString
    for (i <- 0 to sizeX) {
      temp += (abc + "\t")
      abc = (abc(0) + 1).toChar.toString
    }
    temp += "\n  "
    for (i <- 0 to sizeX) {
      temp += "----"
    }
    temp += "\n"*/
    temp += xAxisString(sizeX)
    for (i <- 0 to sizeY) {
      temp += (i + 1 + "\t|")
      for (j <- 0 to sizeX) {

        /*if (character.length > vectorcountC &&
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
        }*/
        temp += fieldPosReturn(j,i,this.cells,character)
      }
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
        return p.displayname + "\t"
      }
    }

    rocks.foreach{ r =>
      if(r.x == x && r.y == y){
        return r.otype + "\t" //Falls otype Enum, hier toString von otype
      }
    }
    return "X\t"
  }
}

