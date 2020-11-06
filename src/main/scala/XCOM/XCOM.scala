package XCOM
object GameState extends Enumeration {
  type GameState = Value
  val MENU, SUI, SHOOT, END = Value
}
import GameState._
import scala.io._

object XCOM {
  //TODO : var ? val
  val scenario = Scenario()//O-? oriented?



  def main(args: Array[String]): Unit = {
    var scenarioField = new Field(0,0)
    var gameState = MENU
    println("Welcome to Xcom!\nFor more information enter Help\n")
    println("If you want to start, enter a number to choose a scenario  between 1 - " + scenario.ammount)


        //TODO: test scenario
        //TODO: move
        //TODO: shoot
        //TODO: Y/N
        //TODO: info


    while(true){
      val input = readString()
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

  def run(gameState: GameState, cGameField:Field, input:String):(GameState, Field, String) = {
    if(input == "EXIT"){
      return (END,cGameField,"\nThanks for playing!\nGoodbye!\n")
    }
    if(input == "HELP"){
      return (gameState,cGameField,"\nHELP" +
        "\nExit:\t\t\texits the game" +
        "\nMove C,X,Y :\tmove Character(C) to X, Y" +
        "\nInfo C:\t\t\tCurrent status of Character(C)" +
        "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n")
    }
    gameState match{
      case MENU =>{
        menu(cGameField,input)
      }
      case SUI =>{
        (END,cGameField,"\nThanks for playing!\nSUI!\n")
      }
      case SHOOT =>{
        (END,cGameField,"\nThanks for playing!\nshoot!\n")
      }
    }
  }

  def menu(cGameField:Field, input:String):(GameState, Field, String) ={
    val valreadInt = input.forall(_.isDigit)

    if(valreadInt ){
      if(input.toInt >= 1 && input.toInt <= scenario.ammount){//&& read() <= Vector.szenario.length
        return  (SUI,scenario.loadScenario(input.toInt),"You can now enter" +
          "\nMove C,X,Y :\tmove Character(C) to X, Y" +
          "\nInfo C:\t\t\tCurrent status of Character(C)" +
          "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n")
      }
    }
    (MENU,cGameField,"If you want to start, enter a number to choose a scenario  between 1 - " + scenario.ammount)
  }

  def sui(){  }

  def readString() :String = {
    val read = StdIn.readLine().toUpperCase()
    read
  }

  def readInt() :Int = {
    val read = StdIn.readInt()
    read
  }
}