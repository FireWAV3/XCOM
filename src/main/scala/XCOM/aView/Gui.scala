package XCOM.aView
import XCOM.controller.Controller
import XCOM.util.Observer

case class Gui(var c : Controller) extends Observer with UiTrait {
  c.add(this)

  def run(input:String): Unit = {
    println("GUI not implemented")

    sys.exit
  }

  override def update: Unit = {
    println("GUI not implemented")
    sys.exit
  }
}
