package XCOM

object XCOM {
  def main(args: Array[String]): Unit = {

    println("Welcome to Xcom!")

    val sniper = Character("Sniper", 5, 10, 70, 40, 0,"C1", Cell(5, 1, "C1"))
    val tank = Character("Tank", 5, 10, 70, 40, 0,"C2", Cell(4, 4, "C2"))
    val allCharacter = Vector[Character](sniper,tank)


    val field1 = Field(6+1, 20+1, Vector[Cell](
      Cell(5, 0, "R"), Cell(2, 2, "R"), Cell(3, 2, "R"), Cell(3, 6, "R"), Cell(3, 7, "R")),
      allCharacter)


    println(field1.toString)

  }
}

