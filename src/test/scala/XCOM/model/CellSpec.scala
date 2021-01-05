package XCOM.model

import org.scalatest.Matchers._
import org.scalatest.WordSpec
import XCOM.model.FieldStructure._

class CellSpec extends WordSpec{
  "A Cell" should{
    "have a X position" in {
      new Cell(2,5).x should be(2)
    }
    "have a Y position" in {
      new Cell(2,5).y should be(5)
    }
    "have a type" in {
      Cell(2,5,X).otype should be(X)
    }
    "have a default X position" in {
      new Cell().x should be(-10)
    }
    "have a default Y position" in {
      new Cell().y should be(-10)
    }
    "have a default type" in {
      new Cell().otype should be(X)
    }
  }
}
