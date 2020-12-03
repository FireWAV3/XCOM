/*package XCOM.util
import XCOM.model.Field

trait Event{
  def handle()
}

object TeamState {
  var state = team1State

  def handle(e: Event) = {
    e match {
      case team1: Team1Event => state = team1State
      case team2: Team2Event => state = team2State
    }
    state
  }

  def team1State(field : Field , c : Character) = Vector{
    var out = field.character.filter(i => i.side == 1)
    var moved[];


    //TODO code controll rückgabe array team 1
    //mit entfernten character
    //Team 1
  }

  def team2State(field : Field) = Vector{
    //TODO code controll rückgabe array team 2
    //mit entfernten character
    //Team 2
  }
}
*/
