package XCOM.model

object FieldStructure extends Enumeration {
  type FieldStructure = Value
  val X, R, C = Value
}
import XCOM.model.FieldStructure._

case class Cell(x: Int, y: Int, otype: FieldStructure){

  def this(){
    this(-10, -10, X)
  }

  def this(x:Int, y:Int){
    this(x, y, X)
  }
}
