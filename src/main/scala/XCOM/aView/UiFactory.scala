package XCOM.aView
import XCOM.controller.Controller

trait UiTrait{
  def run(input:String)
}

object UiTrait {
  def apply(str: String, c: Controller) = str match{
    case "GUI" =>  Gui(c)
    case "TUI" =>  Tui(c)
  }
}
