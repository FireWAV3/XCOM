import org.scalatest.Matchers._
import org.scalatest.WordSpec
import XCOM._
import GameState._
import FieldStructure._

class XcomSpec extends WordSpec{
  "Xcom" should{
    "have a methode menu" in{
      XCOM.menu(new Field(5,5),"a")._1 should be(MENU)

      XCOM.menu(new Field(5,5),"1")._1 should be(SUI)
      XCOM.menu(new Field(5,5),"1")._2 shouldBe a [Field]
      XCOM.menu(new Field(5,5),"1")._3 should include("You can now enter")
    }
    "have a methode move" in{
      XCOM.move(new Character(),new Field(Vector[Character](new Character())),1,1) shouldBe a [Field]

      XCOM.move(new Character(),new Field(Vector[Character](Character("Test", 2, 2, 0, 1, 0,"TT", Cell(0, 0, C)))),1+1,1+1).character should
        be(Vector[Character](Character("Test", 2, 2, 0, 1, 0,"TT", Cell(1, 1, C))))
    }
    "have a methode testRock" in{
      XCOM.testRock(new Field(Vector[Cell](Cell(1,1,R)),""),1+1,1+1) should be(true)
      XCOM.testRock(new Field(Vector[Cell](Cell(1,1,R)),""),0+1,0+1) should be(false)
    }
    "have a methode testHero" in{
      XCOM.testHero(new Field(Vector[Character](new Character())),0+1,0+1) should be(true)
      XCOM.testHero(new Field(Vector[Character](new Character())),1+1,1+1) should be(false)
    }
    "have a methode movePossible" in {
      val tempHero = new Character("Test", 2, 2, 0, 1, 0,"TT", Cell(5, 5, C))
      val tempField = new Field(11,11)
      //right
      XCOM.movePossible(tempHero,tempField,6,8) should be(true)
      XCOM.movePossible(tempHero,tempField,6,9) should be(false)
      //left
      XCOM.movePossible(tempHero,tempField,6,4) should be(true)
      XCOM.movePossible(tempHero,tempField,6,3) should be(false)
      //down
      XCOM.movePossible(tempHero,tempField,8,6) should be(true)
      XCOM.movePossible(tempHero,tempField,9,6) should be(false)
      //up
      XCOM.movePossible(tempHero,tempField,4,6) should be(true)
      XCOM.movePossible(tempHero,tempField,3,6) should be(false)
      //diagonal down right
      XCOM.movePossible(tempHero,tempField,7,7) should be(true)
      XCOM.movePossible(tempHero,tempField,7,8) should be(false)
      XCOM.movePossible(tempHero,tempField,8,8) should be(false)
      XCOM.movePossible(tempHero,tempField,8,7) should be(false)

    }
    "have a methode splitFlatString" in {
      XCOM.splitFlatString("4,B,A,A")(0) should be("4")
      XCOM.splitFlatString("4,C,B,A")(1) should be("C")
      XCOM.splitFlatString("4,C,B,A")(2) should be("B")
      XCOM.splitFlatString("4,C,B,A")(3) should be("A")
    }
    "have a methode testInt" in {
      XCOM.testInt("123") should be(true)
      XCOM.testInt("a13") should be(false)
    }
    "have a methode sui" in{
      val tempField = Scenario().loadScenario(1)
      //move
      XCOM.sui(tempField,"MOVE C1,5,2")._3 should be("Move successful!")
      XCOM.sui(tempField,"MOVE C2,6,20")._3 should be("Move not possible. Target out of range")
      //Info
      XCOM.sui(tempField,"INFO C2")._3 should include("The Character 'Tank'")
      XCOM.sui(tempField,"INFO ABBA")._3 should include("does not exist")
    }
    "have a methode abctoInt" in {
      XCOM.abctoInt("A") should be(0)
      XCOM.abctoInt("B") should be(1)
      XCOM.abctoInt("F") should be(5)
    }
    "have a methode testABC" in {
      XCOM.testABC(new Field(5,5),"A") should be(true)
      XCOM.testABC(new Field(5,5),"F") should be(false)
    }
    "have a methode run" in {
      XCOM.run(MENU,new Field(5,5),"EXIT")._3 should include("Goodbye")
      XCOM.run(MENU,new Field(5,5),"HELP")._3 should include("HELP")
    }
  }
}
