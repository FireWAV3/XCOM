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


