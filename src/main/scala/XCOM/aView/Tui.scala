package XCOM.aView

import XCOM.controller.Controller
import XCOM.controller.GameState._
import XCOM.util.Observer

case class Tui(var c : Controller) extends Observer{
  c.add(this)

  println("Welcome to Xcom!\nFor more information enter Help\n")
  println("If you want to start, enter 'Load,Number' to choose a scenario with Number  between 1 and " + c.scenarioAmmount)

  def processInputLine(input: String): Boolean ={
    if(input.length > 0){
      run(input)
    }
    c.gameState != END
  }

  def run(input:String): Unit = {
    val comInput =  c.splitFlatString(input)
    comInput(0) match {
      case "EXIT" => c.exit
      case "HELP" => c.help
      case "LOAD" => if(comInput.length == 2) load(comInput(1)) else c.wrongInput(input)
      case "INFO" => if(comInput.length == 2) c.info(comInput(1)) else c.wrongInput(input)
      case "SHOOT" => if(comInput.length == 3) c.aim(comInput(1),comInput(2)) else c.wrongInput(input)
      case "MOVE" => if(comInput.length == 4) move(comInput(1),comInput(2),comInput(3)) else c.wrongInput(input)
      case "YES" | "Y" | "NO" | "N" =>  if(comInput(0) == "YES" || comInput(0) == "Y") c.shoot(true) else c.shoot(false)
      case _ => c.wrongInput(input)
    }
  }

  def load(input :String): Boolean ={
    if(c.testInt(input)){
      if(input.toInt >= 1 && input.toInt <= c.scenarioAmmount){
        c.loadScenario(input.toInt)
        return true
      }
    }
    c.wrongInput(input)
    false
  }

  def move(str: String, str1: String, str2: String): Boolean = {
    if(c.testABC(str1)){
      if(c.testInt(str2)){
        c.move(str, c.abcToInt(str1), str2.toInt)
        return true
      }
    }
    c.wrongInput(str +" "+ str1 +" " +str2 )
    false
  }

  override def update: Unit = {
    if(c.gameState == END){
      println("\nThanks for playing!\nGoodbye!\n")
    }else if(c.gameState == SHOOT || c.gameState == SINGLEOUT || c.gameState == MENU){
      println(c.output)
    }
    else if(c.gameState == HELP){
      println("\nHELP" +
              "\nExit:\t\t\tExits the game" +
              "\nMove C,X,Y :\tMove Character(C) to X, Y" +
              "\nInfo C:\t\t\tCurrent status of Character(C)" +
              "\nShoot C,T:\t\tCharacter(C) attacks Target(T)\n")
    }else{
      println(c.fieldToString)
      println(c.output)
    }
  }
}
