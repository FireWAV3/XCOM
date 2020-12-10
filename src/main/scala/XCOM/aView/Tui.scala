package XCOM.aView
import XCOM.controller._
import XCOM.util.{Observer, UndoManager}

import scala.util.{Failure, Success, Try}
//import scala.util.{Try, Success, Failure}

case class Tui(var c : Controller) extends Observer with UiTrait{

  c.add(this)
  val uManager = UndoManager()
  println("Welcome to Xcom!\nFor more information enter Help\n")
  println("If you want to start, enter 'Load,Number' to choose a scenario with Number  between 1 and " + c.scenarioAmmount)


  def run(input:String): Unit = {
    val comInput =  this.c.splitFlatString(input)
    if(comInput.length > 0){
      comInput(0) match {
        case "EXIT" => c.exit
        case "HELP" | "H" =>  c.help
        case "NEXT" => {
          uManager.doStep(c)
          c.next
        }
        case "UNDO" | "U" => {
          c.undo(uManager)
        }
        case "REDO" | "R" => {
          c.redo(uManager)
        }
        case "LOAD" | "L" => {
          if(!load(comInput.lift(1))){
            c.wrongInput(input)
          }
        }
        case "INFO" | "I" => {
          if(!c.info(comInput.lift(1))){
            c.wrongInput(input)
          }
        }
        case "SHOOT" | "S" => {
          if(!c.aim(comInput.lift(1), comInput.lift(2))){
            c.wrongInput(input)
          }
        }
        case "MOVE"  | "M" => {
          uManager.doStep(c)
          if(!move(comInput.lift(1), comInput.lift(2), comInput.lift(3))){
            c.wrongInput(input)
          }
        }
        case "YES" | "Y"  => {
          uManager.doStep(c)
          c.shoot(true)
        }
        case "NO" | "N" => {
          c.shoot(false)
        }
        case _ => c.wrongInput(input)
      }
    }
  }

  def load(input: Option[String]): Boolean ={
    input match {
      case Some(s) => {
        val isValid = c.scenarioAmmountTest(Try(s.toInt))
        isValid match{
          case Success(value) => {
            if(value) c.loadScenario(s.toInt) else false
          }
          case Failure(exception) => false
        }
      }
      case None => false
    }
  }

  def move(str: Option[String], str1: Option[String], str2: Option[String]): Boolean = {
    str2 match {
      case Some(s) => {
        if(c.testABC(str1.get)) {
          Try(c.move(str.get, c.abcToInt(str1.get), str2.get.toInt)) match {
            case Success(value) => return true
          }
        }
        false
      }
      case None => {
       false
      }
    }
  }

  override def update: Unit = {
    println(this.c.fieldToString)
    println(this.c.output)

  }
}
