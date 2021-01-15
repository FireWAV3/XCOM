


val i = 50
val x = "top"

println(raw"test \t t \n print $i $x 1+1 = ${1+1}")
println(f"test print $i $x 1+1 = ${1+1}")




var redoStack : List[String]= Nil

redoStack

redoStack = "fghjk"::redoStack

redoStack = "ghjkl"::redoStack

def testS (input: Option[String])={
  input match {
    case Some(s) => println(input.get +" jo")
    case None => println(" found none")
  }
}

testS(redoStack.lift(1))

