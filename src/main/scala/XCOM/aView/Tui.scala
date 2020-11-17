package XCOM.aView

import XCOM.controller.GameState._
import XCOM.controller.{Controller, GameState}
import XCOM.model.{AttackScenario, Character, Field, Scenario}
import XCOM.util.Observer

case class Tui(var c : Controller) extends Observer{
  c.add(this)

  println("Welcome to Xcom!\nFor more information enter Help\n")
  println("If you want to start, enter 'Load,Number' to choose a scenario with Number  between 1 and " + c.loadScenario(0))

  def processInputLine(input: String): Boolean ={
    if(input.length > 0){
      run(input)
    }
    c.gameState != END
  }

  def load(input :String): Boolean ={
    if(c.testInt(input)){
      if(input.toInt >= 1 && input.toInt <= c.loadScenario(0)){
        c.loadScenario(input.toInt)
        return true
      }
    }
    false
  }


  def move(str: String, str1: String, str2: String): Boolean = {
    //TODO in c + übergabe von move in c
  }

  def run(input:String): Unit = {
    val comInput =  c.splitFlatString(input)

    comInput(0) match {
      case "EXIT" => c.exit
      case "HELP" => c.help
      case "LOAD" => if(comInput.length == 2) load(comInput(1)) else c.wrongInput(input)
      case "INFO" => if(comInput.length == 2) c.info(comInput(1)) else c.wrongInput(input)
      case "SHOOT" => if(comInput.length == 3) c.aim(comInput(1),comInput(2)) else c.wrongInput(input)//TODO in c
      case "MOVE" => if(comInput.length == 3) move(comInput(1),comInput(2),comInput(3)) else c.wrongInput(input)//TODO in move
    }






//
//    gameState match{
//      case MENU =>{
//        val newReturn = menu(cGameField,input)
//        (newReturn._1, newReturn._2, newReturn._3, attack)
//      }
//      case SUI =>{
//        sui(cGameField,input)
//      }
//      case SHOOT =>{
//        shoot(cGameField,input,attack)
//      }
//    }


  }

  def menu(cGameField:Field, input:String):(GameState, Field, String) ={
    val valreadInt = c.testInt(input)
    if(valreadInt){
      if(input.toInt >= 1 && input.toInt <= c.laodScenario(0)._2){//&& read() <= Vector.szenario.length
        return  (SUI,c.laodScenario(input.toInt)._1,"You can now enter" +
          "\nMove C,X,Y :\tMove Character(C) to X, Y" +
          "\nInfo C:\t\t\tCurrent status of Character(C)" +
          "\nshoot C,T:\t\tCharacter(C) attacks Target(T)\n")
      }
    }
    (MENU,cGameField,"If you want to start, enter a number to choose a scenario  between 1 - " + c.laodScenario(0)._2)
  }

  def shoot(cGameField: Field, input: String, attack: AttackScenario):(GameState, Field, String, AttackScenario)  ={
    if (input == "YES" || input == "Y"){
      val random = scala.util.Random
      val randInt = random.nextInt(101)
      if (randInt <= attack.probability){
        val retGameField = c.fire(cGameField,attack.attHero,attack.defHero)
        return (SUI, retGameField._1, attack.attHero.name + "(" + attack.attHero.displayname + ") dealt "
          + retGameField._2 + " damage to " + attack.defHero.name + "(" + attack.defHero.displayname + ")("
          + retGameField._3 + " hp left)", new AttackScenario())
      }
      return (SUI, cGameField, "Shot missed by " +(randInt-attack.probability) + " cm", new AttackScenario())
    } else if (input == "NO" || input == "N"){
      return (SUI, cGameField, "Shot canceled", new AttackScenario())
    }
    (SHOOT, cGameField, "Please enter 'Yes' or 'No'",attack)
  }

  def sui(cGameField:Field, input:String):(GameState, Field, String, AttackScenario) ={
    val comInput =  c.splitFlatString(input)

    if(comInput(0) == "MOVE" && comInput.length == 4){
      var tempCharacter = ""
      var aktHero = new Character()//TODO c
      for (e <- cGameField.character if e.displayname == comInput(1) ){(tempCharacter = comInput(1),aktHero = e)  }
      if(tempCharacter.length > 0){
        if(c.testABC(cGameField,comInput(2)) && c.abctoInt(comInput(2)) -1 <= cGameField.sizeX){
          if(c.testInt(comInput(3)) && comInput(3).toInt - 1 <= cGameField.sizeY){
            if(!c.testRock(cGameField,c.abctoInt(comInput(2)),comInput(3).toInt)){
              if(!c.testHero(cGameField,c.abctoInt(comInput(2)),comInput(3).toInt)){
                if(c.movePossible(aktHero, cGameField,c.abctoInt(comInput(2)), comInput(3).toInt)){

                  return (SUI, c.move(aktHero,cGameField,c.abctoInt(comInput(2)),comInput(3).toInt),"Move successful!", new AttackScenario())

                }else{
                  return (SUI,cGameField,"Move not possible. Target out of range", new AttackScenario())
                }
              }else{
                return (SUI,cGameField,"Move not possible. There is another Character at "+comInput(2)+","+ comInput(3), new AttackScenario())
              }
            }else{
              return (SUI,cGameField,"Move not possible. There is a Rock at "+comInput(2)+","+ comInput(3), new AttackScenario())
            }
          }else{
            return (SUI,cGameField,"the Y Coordinate '"+ comInput(3) +"' is wrong", new AttackScenario())
          }
        }else{
          return (SUI,cGameField,"the X Coordinate '"+ comInput(2) +"' is wrong", new AttackScenario())
        }
      }else{
        return (SUI,cGameField,"the Character '"+ comInput(1) +"' does not exist", new AttackScenario())
      }

    }else  if(comInput(0) == "INFO" && comInput.length == 2){
      var tempCharacter = ""
      var aktHero = new Character()//TODO c
      for (e <- cGameField.character if e.displayname == comInput(1) ){(tempCharacter = comInput(1),aktHero = e)}
      if(tempCharacter.length > 0){
        return (SUI, cGameField,"The Character '" + aktHero.name + "'(" + aktHero.displayname + ", Team "
          + aktHero.side + ") can move over " + aktHero.mrange + " and shoot over " + aktHero.srange
          + " tiles. He has " + aktHero.hp + " health points left.", new AttackScenario())
      }else{
        return (SUI,cGameField,"the Character '"+ comInput(1) +"' does not exist", new AttackScenario())
      }
    }else  if(comInput(0) == "SHOOT" && comInput.length == 3){
      var tempCharacter1 = ""
      var tempCharacter2 = ""
      var aktHero1 = new Character()
      var aktHero2 = new Character()
      for (e <- cGameField.character){
        if (e.displayname == comInput(1)){
          tempCharacter1 = comInput(1)
          aktHero1 = e
        } else if (e.displayname == comInput(2)){
          tempCharacter2 = comInput(2)
          aktHero2 = e
        }
      }
      if (tempCharacter1.length > 0 && tempCharacter2.length > 0 && tempCharacter1 != tempCharacter2){
        val percentage = c.shootpercentage(cGameField,aktHero1,aktHero2)
        return (SHOOT,cGameField,"The chance to hit " + aktHero2.name + "(" + aktHero2.displayname+ ") with "
          + aktHero1.name + "(" + aktHero1.displayname + ") is: " + percentage
          + "%. If you want to shoot, enter 'Yes' otherwise enter 'No'",
          AttackScenario(aktHero1, aktHero2, percentage))
      } else {
        return (SUI,cGameField,"Please enter two valid Characters", new AttackScenario())
      }
    }
    (SUI,cGameField,"Wrong command or wrong attributes", new AttackScenario())
  }

  override def update: Unit = {
    if(c.gameState == END){
      println("\nThanks for playing!\nGoodbye!\n")
    }else if(c.gameState == SHOOT){
      println(c.output)
    }else if(c.gameState == SINGLEOUT){
      println(c.output)
    }else if(c.gameState == HELP){
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
