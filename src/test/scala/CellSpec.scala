import XCOM.Cell
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import XCOM.FieldStructure._

class CellSpec extends WordSpec{
  "A Cell" should{
    "has a X position" in {
      new Cell(2,5).x should be(2)
    }
    "has a Y position" in {
      new Cell(2,5).y should be(5)
    }
    "has a type" in {
      new Cell(2,5,X).otype should be(X)
    }

    "has a default X position" in {
      new Cell().x should be(-10)
    }
    "has a default Y position" in {
      new Cell().y should be(-10)
    }
    "has a default type" in {
      new Cell().otype should be(X)
    }
  }
}
