package XCOM.model.Scenarios.xml
import XCOM.model.{Cell, Character, Field, Scenario}
import XCOM.model.FieldStructure.{C, R}

import scala.collection.mutable.ListBuffer

class ScenarioXML extends Scenario{

  val file = scala.xml.XML.loadFile("src/main/scala/XCOM/model/Scenarios/Scenario.XML")
  val fields = (file \\ "Scenario")


  override def loadScenario(i: Int): Field = {
    val scenario = fields(i)

    println("using XML")

    val nCharacter = (scenario \\ "Character")
    val character = new ListBuffer[Character]()
    var characterNumber = 1;
    nCharacter.foreach{n=>
      character.append(Character((n\"Name").text, (n\"movementRange").text.toInt, (n\"shootingRange").text.toInt,
        (n\"damage").text.toInt, (n\"hp").text.toInt, (n\"side").text.toInt, f"C$characterNumber" ,
        Cell((n\\"Cell"\"x").text.toInt, (n\\"Cell"\"y").text.toInt, C)))
      characterNumber += 1
    }

    val vCharacter = character.toVector
    val nRocks = (scenario \\ "Rock")
    val rocks = new ListBuffer[Cell]()
    nRocks.foreach{ n=>
      rocks.append(Cell((n\\"Cell"\"x").text.toInt, (n\\"Cell"\"y").text.toInt, R))
    }
    val vRocks = rocks.toVector

    Field((scenario \ "sizeX").text.toInt,(scenario \ "sizeY").text.toInt,vRocks,vCharacter)
  }

  override def getAmmount(): Int = fields.length-1
}
