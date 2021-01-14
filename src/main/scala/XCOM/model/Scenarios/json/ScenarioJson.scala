package XCOM.model.Scenarios.json
import XCOM.model.FieldStructure.{C, R}
import XCOM.model.{Cell, Character, Field, Scenario}
import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable.ListBuffer
import scala.io.Source

class ScenarioJson extends Scenario{

  val source: String = Source.fromFile("src/main/scala/XCOM/model/Scenarios/Scenario.json").getLines.mkString
  val json: JsValue = Json.parse(source)
  var jfields =  (json \\ "Scenario")

  override def loadScenario(i: Int): Field = {
    val scenario = jfields(0)(i)

    println("using json")

    val nCharacter = (scenario \\ "Characters")
    val character = new ListBuffer[Character]()
    var characterNumber = 1

    for (n <- nCharacter){
      for(x <- 0 until (scenario \ "ammountCharacter").get.toString().toInt){
        character.append(Character((n(x)\"Name").get.toString().replace("\"",""), (n(x)\"movementRange").get.toString.toInt, (n(x)\"shootingRange").get.toString.toInt,
          (n(x)\"damage").get.toString.toInt, (n(x)\"hp").get.toString.toInt, (n(x)\"side").get.toString.toInt, f"C$characterNumber" ,
          Cell((n(x) \ "Cell" \ "x").get.toString.toInt, (n(x)\"Cell"\"y").get.toString.toInt, C)))
        characterNumber += 1
      }
    }
    val vCharacter = character.toVector
    val nRocks = (scenario \\ "Rocks")
    val rocks = new ListBuffer[Cell]()
    nRocks.foreach{ n=>
      for(x <- 0 until (scenario \ "ammountRocks").get.toString().toInt){
        rocks.append(Cell((n(x)\"Cell"\"x").get.toString.toInt, (n(x)\"Cell"\"y").get.toString.toInt, R))
      }
    }
    val vRocks = rocks.toVector

    Field((scenario \ "sizeX").get.toString.toInt,(scenario \ "sizeY").get.toString.toInt,vRocks,vCharacter)
  }

  override def getAmmount(): Int = (json \ "ammount").get.toString.toInt
}
