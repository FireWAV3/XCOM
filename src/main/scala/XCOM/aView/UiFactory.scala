package XCOM.aView
import XCOM.controller.Controller
import XCOM.util.UndoManager

trait UiTrait{
  def run(input:String)
}

object UiTrait {
  def apply(str: String, c: Controller, uManager: UndoManager) = str match{
    case "GUI" =>  Gui(c)
    case "TUI" =>  Tui(c, uManager)
  }
}
