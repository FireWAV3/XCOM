package XCOM.controller


object  PlayerStatus extends Enumeration {
  type PlayerStatus = Value
  val BLUE, RED = Value

  val map = Map[PlayerStatus, Int](
    BLUE -> 0,
    RED -> 1
  )

  def turn(side: PlayerStatus) = map(side)

}