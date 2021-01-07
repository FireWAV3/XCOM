package XCOM.aView
import XCOM.controller.controllerComponent._
import XCOM.util.UndoManager

import scala.swing.Reactor
import scala.util.{Failure, Success, Try}

case class Tui(var c : ControllerInterface, uManager: UndoManager) extends Reactor with UiTrait {

  listenTo(c)
  println("Welcome to Xcom!\nFor more information enter Help\n")
  println("If you want to start, enter 'Load,Number' to choose a scenario with Number  between 1 and " + c.scenarioAmmount)


  def run(input: String): Unit = {
    val comInput = this.c.splitFlatString(input)
    if (comInput.length > 0) {
      comInput(0) match {
        case "EXIT" => c.exit
        case "HELP" | "H" => c.help
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
          if (!load(comInput.lift(1))) {
            c.wrongInput(input)
          }
        }
        case "INFO" | "I" => {
          if (!c.info(comInput.lift(1))) {
            c.wrongInput(input)
          }
        }
        case "SHOOT" | "S" => {
          if (!c.aim(comInput.lift(1), comInput.lift(2))) {
            c.wrongInput(input)
          }
        }
        case "MOVE" | "M" => {
          uManager.doStep(c)
          if (!move(comInput.lift(1), comInput.lift(2), comInput.lift(3))) {
            c.wrongInput(input)
            uManager.undoClear(c)
          }
        }
        case "YES" | "Y" => {
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

  def load(input: Option[String]): Boolean = {
    input match {
      case Some(s) => {
        val isValid = c.scenarioAmmountTest(Try(s.toInt))
        isValid match {
          case Success(value) => {
            if (value) c.loadScenario(s.toInt) else false
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
        if (c.testABC(str1.get)) {
          Try(c.move(str.get, c.abcToInt(str1.get), str2.get.toInt)) match {
            case Success(value) => return true
            case Failure(exception) => return false
          }
        }
        false
      }
      case None => {
        false
      }
    }
  }

  reactions += {
    case event: UpdateField => printField
    case event: UpdateText => printOut
    case event: UpdateInfo => printOut
    case event: UpdateHelp => printHelp
    case event: UpdateShoot => printAim
    //case event: UpdateMenu => printField
  }

  def printField: Unit = {
    println(c.fieldToString)
    println(c.output)
  }

  def printOut: Unit = println(c.output)

  def printAim: Unit = println(c.output + " If you want to shoot enter 'Yes' otherwise 'No'")

  def printHelp: Unit = println("\nHELP" +
    "\nExit:\t\t\tExits the game" +
    "\nMove C,X,Y :\tMove Character(C) to X, Y" +
    "\nInfo C:\t\t\tCurrent status of Character(C)" +
    "\nShoot C,T:\t\tCharacter(C) attacks Target(T)\n"+
    "\nUndo:\t\tMoves you back in Time to the last step you made\n"+
    "\nRedo:\t\treverts the Undo Time travel\n")
}