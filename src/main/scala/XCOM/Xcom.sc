

case class Cell(x:Int, y:Int, otype:String)
case class Character(name:String, mrange:Int, srange:Int, damage:Int, hp:Int, side:Int, displayname:String,cell:Cell)

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
  return tempReturn
}

case class Field(pX:Int, pY:Int, cells:Vector[Cell], character: Vector[Character]){
  var sizeX = pX-1
  var sizeY = pY-1

  override def toString: String = {
    var vectorcountF, vectorcountC = 0
    var temp = "\t "
    /*var abc = "A" //replaced by xAxisString
    for(i <- 0 to sizeX){
      temp += (abc+ "\t")
      abc = (abc(0) + 1).toChar.toString
    }
    temp += "\n  "
    for (i <- 0 to sizeX){
      temp += "----"
    }
    temp += "\n"*/
    temp += xAxisString(sizeX)
    for (i <- 0 to  sizeY){
      temp += (i+1 + "\t|")
      for (j <- 0 to  sizeX){
        if(character.length > vectorcountC &&
          character(vectorcountC).cell.x == j  &&
          character(vectorcountC).cell.y == i){
          temp += this.character(vectorcountC).displayname
          temp += "\t"
          vectorcountC += 1
        }else if(cells.length > vectorcountF &&
          cells(vectorcountF).x == j  &&
          cells(vectorcountF).y == i){
          temp += this.cells(vectorcountF).otype
          temp += "\t"
          vectorcountF += 1
        }else{
          temp += "X\t"
        }
      }
      temp += "\n"
    }
    return temp
  }
}





//start little test
val sniper = Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 1, "C1"))
val tank = Character("Tank", 5, 10, 70, 40, 0,"C2", Cell(4, 4, "C2"))
val allCharacter = Vector[Character](sniper,tank)


val field1 = Field(6+1, 20+1, Vector[Cell](
  Cell(5, 0, "R"), Cell(2, 2, "R"), Cell(3, 2, "R"), Cell(3, 6, "R"), Cell(3, 7, "R")),
  allCharacter)


println(field1.toString)
