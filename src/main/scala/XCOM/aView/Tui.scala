package XCOM.aView
import XCOM.controller._
import XCOM.util.Observer
import XCOM.util.UndoManager

case class Tui(var c : Controller) extends Observer with UiTrait{

  c.add(this)
  val uManager = UndoManager()
  println("Welcome to Xcom!\nFor more information enter Help\n")
  println("If you want to start, enter 'Load,Number' to choose a scenario with Number  between 1 and " + c.scenarioAmmount)


  def run(input:String): Unit = {
    val comInput =  this.c.splitFlatString(input)
    if(comInput.length > 0){

      comInput(0) match {
        //TODO Next command
        case "EXIT" => c.exit
        case "HELP" =>  c.help
        case "NEXT" => {
          uManager.doStep(c)
          c.next
        }
        case "UNDO" => {
          c.undo(uManager)
        }
        case "REDO" => {
          c.redo(uManager)
        }
        case "LOAD" => {
          if (comInput.length == 2) {
            load(comInput(1))
          } else c.wrongInput(input)
        }
        case "INFO" => {
          if (comInput.length == 2) {
            c.info(comInput(1))
          } else c.wrongInput(input)
        }
        case "SHOOT" => {
          if (comInput.length == 3) {
            c.aim(comInput(1), comInput(2))
          } else c.wrongInput(input)
        }
        case "MOVE" => {
          uManager.doStep(c)
          if (comInput.length == 4) {
            move(comInput(1), comInput(2), comInput(3))
          } else c.wrongInput(input)
        }
        case "YES" | "Y" | "NO" | "N" => {
          if (comInput(0) == "YES" || comInput(0) == "Y") {
            uManager.doStep(c)
            c.shoot(true)
          }else c.shoot(false)

        }
        case _ => c.wrongInput(input)
      }
    }
  }

  def load(input :String): Boolean ={
    if(c.testInt(input)){
      if(input.toInt >= 0 && input.toInt <= c.scenarioAmmount){
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
    println(this.c.fieldToString)
    println(this.c.output)

  }
}
