package XCOM.controller

object  GameStatus extends Enumeration {
  type GameState = Value
  val MENU, SUI, SHOOT, END , HELP ,SINGLEOUT = Value
}


object  PlayerStatus extends Enumeration {
  type PlayerStatus = Value
  val BLUE, RED = Value

  val map = Map[PlayerStatus, Int](
    BLUE -> 0,
    RED -> 1
  )

  def turn(side: PlayerStatus) = map(side)

}