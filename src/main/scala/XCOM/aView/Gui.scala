package XCOM.aView
import XCOM.controller.Controller
import XCOM.util.Observer

case class Gui(var c : Controller) extends Observer with UiTrait {
  c.add(this)

  def run(input:String): Unit = {
    throw new Exception("GUI not implemented")
  }

  override def update: Unit = {
    throw new Exception("GUI not implemented")
  }
}
