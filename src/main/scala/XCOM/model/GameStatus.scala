package XCOM.model

//Used to keep track of which team currently is in charge
object  PlayerStatus extends Enumeration {
  type PlayerStatus = Value
  val BLUE, RED = Value

  val map = Map[PlayerStatus, Int](
    BLUE -> 0,
    RED -> 1
  )

  def turn(side: PlayerStatus) = map(side)

}