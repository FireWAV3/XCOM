package XCOM
object GameState extends Enumeration {
  type GameState = Value
  val MENU, SUI, SHOOT, END = Value
}
import GameState._
import scala.io._
import scala.collection.mutable.ListBuffer
import FieldStructure._

object XCOM {
  //TODO : var ? val
  val scenario = Scenario()



  def main(args: Array[String]): Unit = {
    var scenarioField = new Field(0,0)
    var gameState = MENU
    println("Welcome to Xcom!\nFor more information enter Help\n")
    println("If you want to start, enter a number to choose a scenario  between 1 - " + scenario.amount)


        //TODO: test scenario
        //TODO: shoot
        //TODO: Y/N


    while(true){
      val input = readString()
      if(input.length > 0){
        val runT = run(gameState, scenarioField, input)
        gameState = runT._1
        scenarioField= runT._2
        if(gameState == END){
          println(runT._3)
          return
        }else{
          println(scenarioField)
          println(runT._3)
        }
      }
    }

  }

  def run(gameState: GameState, cGameField:Field, input:String):(GameState, Field, String) = {
    if(input == "EXIT"){
      return (END,cGameField,"\nThanks for playing!\nGoodbye!\n")
    }
    if(input == "HELP"){
      return (gameState,cGameField,"\nHELP" +
        "\nExit:\t\t\tExits the game" +
        "\nMove C,X,Y :\tMove Character(C) to X, Y" +
        "\nInfo C:\t\t\tCurrent status of Character(C)" +
        "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n")
    }
    gameState match{
      case MENU =>{
        menu(cGameField,input)
      }
      case SUI =>{
        sui(cGameField,input)
      }
      case SHOOT =>{
        (END,cGameField,"\nThanks for playing!\nshoot!\n")
      }
    }
  }

  def menu(cGameField:Field, input:String):(GameState, Field, String) ={
    val valreadInt = testInt(input)
    if(valreadInt ){
      if(input.toInt >= 1 && input.toInt <= scenario.amount){//&& read() <= Vector.szenario.length
        return  (SUI,scenario.loadScenario(input.toInt),"You can now enter" +
          "\nMove C,X,Y :\tMove Character(C) to X, Y" +
          "\nInfo C:\t\t\tCurrent status of Character(C)" +
          "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n")
      }
    }
    (MENU,cGameField,"If you want to start, enter a number to choose a scenario  between 1 - " + scenario.amount)
  }

  def sui(cGameField:Field, input:String):(GameState, Field, String) ={
    val comInput =  splitFlatString(input)

    if(comInput(0) == "MOVE" && comInput.length == 4){
      var tempCharacter = ""
      var aktHero = new Character()
      for (e <- cGameField.character if e.displayname == comInput(1) ){(tempCharacter = comInput(1),aktHero = e)  }
      if(tempCharacter.length > 0){
        if(testABC(cGameField,comInput(2)) && abctoInt(comInput(2)) -1 <= cGameField.sizeX){
          if(testInt(comInput(3)) && comInput(3).toInt - 1 <= cGameField.sizeY){
            if(!testRock(cGameField,abctoInt(comInput(2)),comInput(3).toInt)){
              if(!testHero(cGameField,abctoInt(comInput(2)),comInput(3).toInt)){
                if(movePossible(aktHero, cGameField,abctoInt(comInput(2)), comInput(3).toInt)){

                  return (SUI, move(aktHero,cGameField,abctoInt(comInput(2)),comInput(3).toInt),"Move successful!")

                }else{
                  return (SUI,cGameField,"Move not possible. Target out of range")
                }
              }else{
                return (SUI,cGameField,"Move not possible. There is another Character at "+comInput(2)+","+ comInput(3))
              }
            }else{
              return (SUI,cGameField,"Move not possible. There is a Rock at "+comInput(2)+","+ comInput(3))
            }
          }else{
            return (SUI,cGameField,"the Y Coordinate '"+ comInput(3) +"' is wrong")
          }
        }else{
          return (SUI,cGameField,"the X Coordinate '"+ comInput(2) +"' is wrong")
        }
      }else{
        return (SUI,cGameField,"the Character '"+ comInput(1) +"' does not exist")
      }

    }else  if(comInput(0) == "INFO" && comInput.length == 2){
      var tempCharacter = ""
      var aktHero = new Character()
      for (e <- cGameField.character if e.displayname == comInput(1) ){(tempCharacter = comInput(1),aktHero = e)}
      if(tempCharacter.length > 0){
          return (SUI, cGameField,"The Character '" + aktHero.name + "'(" + aktHero.displayname + ", Team "
            + aktHero.side + ") can move over " + aktHero.mrange + " and shoot over " + aktHero.srange
            + " tiles. He has " + aktHero.hp + " health points left.")
      }else{
        return (SUI,cGameField,"the Character '"+ comInput(1) +"' does not exist")
      }
    }else  if(comInput(0) == "SHOOT"){
        //TODO Shoot
    }
    (SUI,cGameField,"Wrong command or wrong attributes")
  }

  def move(hero:Character, cGameField:Field, pX:Int, pY:Int):Field = {
    var newCharacterV = ListBuffer[Character]()
    for(e <- cGameField.character){
      if(e.displayname == hero.displayname){
        newCharacterV += Character(e.name,e.mrange,e.srange,e.damage,e.hp,e.side,e.displayname,Cell(pX-1, pY-1, C))
      }else{
        newCharacterV += e
      }
    }
    Field(cGameField.pX,cGameField.pY,cGameField.rocks,newCharacterV.toVector)
  }

  def testRock(cGameField:Field, pX: Int, pY: Int): Boolean = {
    for(e <- cGameField.rocks if e.x == (pX-1)  if e.y == (pY-1) ) return true
    false
  }

  def testHero(cGameField: Field, pX: Int, pY: Int): Boolean = {
    for(e <- cGameField.character if e.cell.x == pX-1 && e.cell.y == pY-1)return true
    false
  }

  def movePossible(hero:Character, cGameField:Field, pX:Int, pY:Int):Boolean = {
    //TODO A*
    val xDistance = pX - hero.cell.x
    val yDistance = pY - hero.cell.y
    var distance = 0
    if (xDistance < 0){
      distance = -xDistance + yDistance
    } else if (yDistance < 0){
      distance = xDistance - yDistance
    } else if (xDistance < 0 && yDistance < 0){
      distance = -xDistance - yDistance
    } else {
      distance = xDistance + yDistance
    }
   hero.mrange >= distance
  }

  def splitFlatString(input:String):Array[String] = {
    input.replace(',',' ').split("\\s+")
  }

  def testInt(input:String):Boolean = {
    input.forall(_.isDigit)
  }

  def readString() :String = {
    val read = StdIn.readLine().toUpperCase()
    read
  }

  def testABC(cGameField:Field, str: String):Boolean = {
    if(str.length == 1){
      val chr = str.charAt(0)
      if(chr >= 'A' && chr <= 'A'+cGameField.sizeX){
        return true
      }
    }
    false
  }

  def abctoInt(str: String):Int = {
    val chr = str.charAt(0)
    chr.toInt-65+1
  }

}