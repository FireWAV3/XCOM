package XCOM
import FieldStructure._

case class Character(name:String, mrange:Int, srange:Int, damage:Int, hp:Int, side:Int, displayname:String,cell:Cell){

  def this(){
    this("Test", 0, 0, 0, 1, 0,"TT", Cell(0, 0, C))
  }

}
