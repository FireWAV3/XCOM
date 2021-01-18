package XCOM
import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import model.Scenario
import model.Scenarios.xml._
//import model.Scenarios.json._

//used to switch between loading Scenarios from Json or loading them from XML
class XcomModule extends AbstractModule with ScalaModule{
  override def configure()={

   bind[Scenario].to[ScenarioXML]
   //bind[Scenario].to[ScenarioJson]


  }
}
