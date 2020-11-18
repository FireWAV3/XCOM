package XCOM


import org.scalatest.WordSpec
//import GameState._

class XcomSpec extends WordSpec{
//  "Xcom" should{
//    "have a methode menu" in{
//      _root_.XCOM.XCOM.menu(new Field(5,5),"a")._1 should be(MENU)
//
//      _root_.XCOM.XCOM.menu(new Field(5,5),"1")._1 should be(SUI)
//      _root_.XCOM.XCOM.menu(new Field(5,5),"1")._2 shouldBe a [Field]
//      _root_.XCOM.XCOM.menu(new Field(5,5),"1")._3 should include("You can now enter")
//    }
//    "have a methode move" in{
//      _root_.XCOM.XCOM.move(new model.Character(),new Field(Vector[model.Character](new model.Character())),1,1) shouldBe a [Field]
//
//      _root_.XCOM.XCOM.move(new model.Character(),new Field(Vector[model.Character](Character("Test", 2, 2, 0, 1, 0,"TT", Cell(0, 0, C)))),1+1,1+1).character should
//        be(Vector[model.Character](Character("Test", 2, 2, 0, 1, 0,"TT", Cell(1, 1, C))))
//    }
//    "have a methode testRock" in{
//      _root_.XCOM.XCOM.testRock(new Field(Vector[Cell](Cell(1,1,R)),""),1+1,1+1) should be(true)
//      _root_.XCOM.XCOM.testRock(new Field(Vector[Cell](Cell(1,1,R)),""),0+1,0+1) should be(false)
//    }
//    "have a methode testHero" in{
//      _root_.XCOM.XCOM.testHero(new Field(Vector[model.Character](new model.Character())),0+1,0+1) should be(true)
//      _root_.XCOM.XCOM.testHero(new Field(Vector[model.Character](new model.Character())),1+1,1+1) should be(false)
//    }
//    "have a methode movePossible" in {
//      val tempHero = new model.Character("Test", 2, 2, 0, 1, 0,"TT", Cell(5, 5, C))
//      val tempField = new Field(11,11)
//      //right
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,6,8) should be(true)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,6,9) should be(false)
//      //left
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,6,4) should be(true)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,6,3) should be(false)
//      //down
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,8,6) should be(true)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,9,6) should be(false)
//      //up
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,4,6) should be(true)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,3,6) should be(false)
//      //diagonal down right
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,7,7) should be(true)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,7,8) should be(false)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,8,8) should be(false)
//      _root_.XCOM.XCOM.movePossible(tempHero,tempField,8,7) should be(false)
//
//    }
//    "have a methode splitFlatString" in {
//      _root_.XCOM.XCOM.splitFlatString("4,B,A,A")(0) should be("4")
//      _root_.XCOM.XCOM.splitFlatString("4,C,B,A")(1) should be("C")
//      _root_.XCOM.XCOM.splitFlatString("4,C,B,A")(2) should be("B")
//      _root_.XCOM.XCOM.splitFlatString("4,C,B,A")(3) should be("A")
//    }
//    "have a methode testInt" in {
//      _root_.XCOM.XCOM.testInt("123") should be(true)
//      _root_.XCOM.XCOM.testInt("a13") should be(false)
//    }
//    "have a methode sui" in{
//      val tempField = Scenario().loadScenario(1)
//      //move
//      _root_.XCOM.XCOM.sui(tempField,"MOVE C1,E,2")._3 should be("Move successful!")
//      _root_.XCOM.XCOM.sui(tempField,"MOVE C2,F,20")._3 should be("Move not possible. Target out of range")
//      //Info
//      _root_.XCOM.XCOM.sui(tempField,"INFO C2")._3 should include("The Character 'Tank'")
//      _root_.XCOM.XCOM.sui(tempField,"INFO ABBA")._3 should include("does not exist")
//    }
//    "have a methode abctoInt" in {
//      _root_.XCOM.XCOM.abctoInt("A") should be(1)
//      _root_.XCOM.XCOM.abctoInt("B") should be(2)
//      _root_.XCOM.XCOM.abctoInt("F") should be(6)
//    }
//    "have a methode testABC" in {
//      _root_.XCOM.XCOM.testABC(new Field(5,5),"A") should be(true)
//      _root_.XCOM.XCOM.testABC(new Field(5,5),"F") should be(false)
//    }
//    "have a methode run" in {
//      _root_.XCOM.XCOM.run(MENU,new Field(5,5),"EXIT",new AttackScenario())._3 should include("Goodbye")
//      _root_.XCOM.XCOM.run(MENU,new Field(5,5),"HELP",new AttackScenario())._3 should include("HELP")
//    }
//    "have a methode shoot" in {
//      _root_.XCOM.XCOM.shoot(new Field(5,5),"YES",AttackScenario(Character("Attack",100,100,100,100,0,"A",Cell(5,5,C)),new model.Character(),100))._3 should include("dealt")
//      _root_.XCOM.XCOM.shoot(new Field(5,5),"YES",AttackScenario(Character("Attack",100,100,100,100,0,"A",Cell(5,5,C)),new model.Character(),0))._3 should include("missed")
//      _root_.XCOM.XCOM.shoot(new Field(5,5),"NO",AttackScenario(Character("Attack",100,100,100,100,0,"A",Cell(5,5,C)),new model.Character(),50))._3 should include("canceled")
//    }
//    "have a methode shootpercentage" in{
//      _root_.XCOM.XCOM.shootpercentage(new Field(5,5),Character("Attack",100,100,100,100,0,"A",Cell(5,5,C)),Character("Defender",100,100,100,100,0,"D",Cell(5,4,C))) should be(99)
//      _root_.XCOM.XCOM.shootpercentage(new Field(5,5),Character("Attack",1,1,100,100,0,"A",Cell(5,50,C)),Character("Defender",100,100,100,100,0,"D",Cell(5,4,C))) should be(0)
//    }
//    "have a methode fire" in{
//      val tempHeros = Vector[model.Character](Character("Attack",10,10,50,100,0,"A",Cell(1,1,C)),Character("Defense",10,10,50,100,0,"D",Cell(1,2,C)))
//      _root_.XCOM.XCOM.fire(new Field(tempHeros),tempHeros(0),tempHeros(1))._1.character(1).hp should be(50)
//    }
//  }
}
