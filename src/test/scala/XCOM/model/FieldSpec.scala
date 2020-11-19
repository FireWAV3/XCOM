package XCOM.model
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import XCOM.model.FieldStructure._

class FieldSpec extends WordSpec{

  "A Field" should{
    "have a X size" in {
      new Field(10,20).sizeX should be(9)
    }
    "have a Y size" in {
      new Field(10,20).sizeY should be(19)
    }

    "have a methode xAxisString" in {
      var testField = new Field(6,6)
      testField.xAxisString(5) should include("A\tB\tC\tD\tE\tF")
      testField.xAxisString(5) should not include("G")
    }
    "have a methode printRow" in {
      var testField = new Field(6,6)
      testField.printRow(0) should include("X")
      testField.printRow(0) should not include("C")
      testField.printRow(0) should not include("R")
    }
    "have a methode fieldPosReturn test with Character" in {
      var testField = new Field(Vector[Character](Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 1, C))))
      testField.fieldPosReturn(5,1) should include("C1")
      testField.fieldPosReturn(5,1) should not include("X")
      testField.fieldPosReturn(5,1) should not include("R")
    }
    "have a methode fieldPosReturn test with Rocks" in {
      var testField = new Field(Vector[Cell](Cell(5,0,R)),"")
      testField.fieldPosReturn(5,0) should include("R")
      testField.fieldPosReturn(5,0) should not include("C")
      testField.fieldPosReturn(5,0) should not include("X")
    }
    "have a methode toString" in {
      var testField = new Field(6,6)
      testField.toString() should include("6")
      testField.toString() should not include("7")
      testField = new Field(0,0)
      testField.toString() should be("")
    }
  }

}