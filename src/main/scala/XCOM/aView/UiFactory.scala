package XCOM.aView
import XCOM.controller.controllerComponent._
import XCOM.util.UndoManager

trait UiTrait{
  def run(input:String)
}

//Factory Methode to create TUI or GUI
object UiTrait {
  def apply(str: String, c: ControllerInterface, uManager: UndoManager) = str match{
    case "GUI" =>  Gui(c)
    case "TUI" =>  Tui(c, uManager)
  }
}
