package XCOM

object FieldStructure extends Enumeration {
  type FieldStructure = Value
  val X, R, C = Value
}
import FieldStructure._

case class Cell(xPos: Int, yPos: Int, otype: FieldStructure){
  var  x = xPos
  var  y = yPos

  def this(){
    this(-10, -10, X)
  }

  def this(x:Int, y:Int){
    this(x, y, X)
  }


}
