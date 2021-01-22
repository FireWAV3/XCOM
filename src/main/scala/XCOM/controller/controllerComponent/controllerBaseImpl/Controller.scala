package XCOM.controller.controllerComponent.controllerBaseImpl

import XCOM.controller.controllerComponent._
import XCOM.{XcomModule, model}
import XCOM.model.PlayerStatus.{BLUE, PlayerStatus, RED}
import XCOM.model.{AttackScenario, Character, Field, PlayerStatus, Scenario, TurnScenario}
import XCOM.util.UndoManager
import com.google.inject.{Guice, Inject}
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

case class Controller @Inject() (var field: Field, var attack: AttackScenario) extends ControllerInterface {

  //Parameters of the Controller
  var context = new Context(this)
  var contextTravel = new ContextTravel(this)
  var output = ""
  var seed = 0
  var PlayerState: PlayerStatus = BLUE
  var turnS = TurnScenario()
  val injector = Guice.createInjector(new XcomModule)

  //Constructor for testing
  def this() {
    this(new Field(0, 0), new AttackScenario())
  }

  //Copy the whole Controller
  def deepCopy(): Controller = {
    val Cout = Controller(this.field, this.attack)
    Cout.context = this.context.deepCopy()
    Cout.contextTravel = this.contextTravel.deepCopy()
    Cout.turnS = this.turnS.deepCopy()
    Cout.output = this.output
    Cout.seed = this.seed
    Cout.PlayerState = this.PlayerState
    Cout
  }

  def help: Unit = {
    context.state.help()
  }

  def exit: Unit = {
    context.state.exit("")
  }

  def info(str: Option[String]): Boolean = {
    str match {
      case Some(s) => {
        context.state.info(s)
      }
      case None => false
    }
  }

  def loadScenario(index: Int): Boolean = {
    context.state.loadScenario(index)
  }

  def move(str: String, pX: Int, pY: Int): Boolean = {
    context.state.move(str, pX, pY)
  }

  def aim(str1: Option[String], str2: Option[String]): Boolean = {
    str2 match {
      case Some(s) => {
        context.state.aim(str1.get, str2.get)
        true
      }
      case None => false
    }
  }

  def shoot(approval: Boolean): Boolean = {
    context.state.shoot(approval, seed)
  }

  def next(): Boolean = {
    context.state.next()
  }

  def undo(uManager: UndoManager) = {
    val newC = uManager.undoStep(this)
    //Setting all Parameters to the old state
    field = newC.field
    attack = newC.attack
    turnS = newC.turnS
    context = newC.context
    contextTravel = newC.contextTravel
    output = "Wow, did we just travel back in time?\n "
    seed = newC.seed
    PlayerState = newC.PlayerState
    publish(new UpdateField)
  }

  def redo(uManager: UndoManager) = {
    val newC = uManager.redoStep(this)
    //Setting all Parameters to the new state
    field = newC.field
    attack = newC.attack
    turnS = newC.turnS
    context = newC.context
    contextTravel = newC.contextTravel
    output = "And we're back in the future\n"
    seed = newC.seed
    PlayerState = newC.PlayerState
    publish(new UpdateField)
  }

  //used to execute a successful shot
  def fire(attHero: model.Character, defHero: model.Character): (Int, Int) = {
    if (defHero.hp - attHero.damage <= 0) {//remove Player who died
      val temp = field.character.filter(i => i.displayname != defHero.displayname)
      field = Field(field.pX, field.pY, field.rocks, temp)
    } else {//lower health of hit player
      val temp = field.character.map { i =>
        if (i.displayname == defHero.displayname) {
          Character(i.name, i.mrange, i.srange, i.damage, i.hp - attHero.damage, i.side, i.displayname, i.cell)
        } else {
          i
        }
      }
      field = Field(field.pX, field.pY, field.rocks, temp)
    }

    (attHero.damage, if ((defHero.hp - attHero.damage) > 0) (defHero.hp - attHero.damage) else 0)
  }

  //check if turn is over because everybody shot
  def checkTurn(): Boolean = {
    if (turnS.testEnd()) {
      PlayerState = nextPlayerState(PlayerState)
      turnS.load(PlayerStatus.turn(PlayerState), field)
      output = "Let's go " + PlayerState + ". Time to kick some ass!"
      publish(new UpdateField)
      return true
    }
    false
  }

  def nextPlayerState(side: PlayerStatus): PlayerStatus = {
    if (PlayerState == BLUE) {
      return RED
    }
    BLUE
  }

  //bounds of field on x-Axis
  def boundsX(x: Int): Boolean = {
    if (field.sizeX >= x - 1 && x - 1 >= 0) true else throw new Exception("What is this place you're talking of?")
  }

  //bounds of field on y-Axis
  def boundsY(y: Int): Boolean = {
    if (field.sizeY >= y - 1 && y - 1 >= 0) true else throw new Exception("What is this place you're talking of?")
  }

  //Check if theres a rock on given position. Returns true if theres no rock
  def testRock(pX: Int, pY: Int): Boolean = {
    for (e <- field.rocks if e.x == (pX - 1) if e.y == (pY - 1)) throw new Exception("This rock's to high to climb it")
    true
  }

  //Check if theres a Character on given position. Returns true if theres no Character
  def testHero(pX: Int, pY: Int): Boolean = {
    for (e <- field.character if e.cell.x == pX - 1 && e.cell.y == pY - 1) throw new Exception("I can't stand on his head, can I?")
    true
  }

  //Get the Hero on given position
  def getHero(pX: Int, pY: Int): Character = {
    for (e <- field.character if e.cell.x == pX - 1 && e.cell.y == pY - 1) return e
    throw new Exception()
  }

  //Check if String represents a Character
  def isHero(input: String): Option[Character] = {
    field.character.map(i => if (i.displayname == input) return Some(i))
    None
  }

  def getFieldAsArray(): Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](field.sizeY+1,field.sizeX+1)
    for (e <- field.character) matrix(e.cell.y)(e.cell.x) = 1
    for (e <- field.rocks ) matrix(e.y)(e.x) = 1
    matrix
  }

  //Movement using Breadth-first search
  def aStarMove(hero:model.Character, goalX: Int, goalY: Int): Boolean = {
    val fieldArray = getFieldAsArray()
    //4 possible directions
    val directions = Vector[(Int,Int)]((0,1),(0,-1),(1,0),(-1,0))

    val q = new ListBuffer[(Int,Int)]()
    q.append((hero.cell.y,hero.cell.x))
    val n = fieldArray.length     //N represents Y
    val m = fieldArray(0).length  //M represents X
    val dist =  Array.ofDim[Int](n,m)
    for(a:Int <- 0 to n-1) {
      for (b:Int <- 0 to m-1) {
        dist(a)(b) = -1
      }
    }
    dist(hero.cell.y)(hero.cell.x) = 0
    while(!q.isEmpty){//calculate distance for every not blocked tile on the field
      val p = q.remove(0)
      for(dir <- directions){
        val b = p._1 + dir._1
        val a = p._2 + dir._2
        if((a >= 0) && (b >= 0) && (a < m) && (b < n) && (dist(b)(a) == -1) && (fieldArray(b)(a) == 0) ){
          dist(b)(a) = 1 + dist(p._1)(p._2)
          q.append((b,a))
        }
      }
    }
    //check if distance is in movementrange of character
    if(dist(goalY-1)(goalX-1) > hero.mrange)throw new Exception("That's way too far away. I would never get there")
    true
  }

  //used to switch between Breadth-first search and Manhattan search for movement
  def movePossible(hero: model.Character, pX: Int, pY: Int): Boolean = {
    if (!(hero.side >= 0)) {
      contextTravel.travelState = new Manhattan(this)
    } else {
      contextTravel.travelState = new AStar(this)
    }
    contextTravel.travelState.movePossible(hero, pX, pY)
  }

  //calculate hitpercentage depending on distance, shootingrange and obstacles
  def shootpercentage(attHero: model.Character, defHero: model.Character): Int = {
    //direction of shot
    val globalDirectionVector = (defHero.cell.x - attHero.cell.x,defHero.cell.y - attHero.cell.y)
    //more fields to shoot from when theres a rock next to the attacker in shooting direction
    var attackingField = Vector[(Int,Int)]((attHero.cell.x,attHero.cell.y))
    if (Math.abs(globalDirectionVector._1) > Math.abs(globalDirectionVector._2)){// shooting in x direction
      Try(testRock(attHero.cell.x + globalDirectionVector._1/Math.abs(globalDirectionVector._1) + 1,
        attHero.cell.y + 1)) match {
        case Success(value) =>
        case Failure(exception) => attackingField = Vector[(Int,Int)]((attHero.cell.x,attHero.cell.y),
          (attHero.cell.x,attHero.cell.y-1),(attHero.cell.x,attHero.cell.y+1))
      }
    }else if (Math.abs(globalDirectionVector._2) > Math.abs(globalDirectionVector._1)){// shooting in y direction
      Try(testRock(attHero.cell.x + 1,
        attHero.cell.y + globalDirectionVector._2/Math.abs(globalDirectionVector._2) + 1)) match {
        case Success(value) =>
        case Failure(exception) => attackingField = Vector[(Int,Int)]((attHero.cell.x,attHero.cell.y),
          (attHero.cell.x-1,attHero.cell.y),(attHero.cell.x+1,attHero.cell.y))
      }
    }

    var minDistance = attHero.srange+1
    //calculate distance from every attacking point to every defending point
    for (attField <- attackingField if(Try(testRock(attField._1+1,attField._2+1)) != Failure)) {
      val attackBox: Vector[(Double, Double)] = Vector((attField._1 - 0.5, attField._2 - 0.5), // center and edges of attacker
        (attField._1 - 0.5, attField._2 + 0.5), (attField._1, attField._2),
        (attField._1 + 0.5, attField._2 - 0.5), (attField._1 + 0.5, attField._2 + 0.5))
      val defBox: Vector[(Double, Double)] = Vector((defHero.cell.x - 0.5, defHero.cell.y - 0.5), // center and edges of defender
        (defHero.cell.x - 0.5, defHero.cell.y + 0.5), (defHero.cell.x, defHero.cell.y),
        (defHero.cell.x + 0.5, defHero.cell.y - 0.5), (defHero.cell.x + 0.5, defHero.cell.y + 0.5))

      for (attPoint <- attackBox; //aim from every attackPoint
           defPoint <- defBox) { //aim to every defensePoint
        val directionVector = (defPoint._1 - attPoint._1, defPoint._2 - attPoint._2)
        //calculating the distance
        var distance = 0.0
        if (Math.abs(directionVector._1) == Math.abs(directionVector._2)) {//45° shooting
          distance = Math.abs(directionVector._1)
        } else {
            distance = Math.abs(directionVector._1) + Math.abs(directionVector._2)
        }

        val m :Double = directionVector._2 / directionVector._1 //m and c of formula y=m*x+c
        val c :Double = attPoint._2 - m * attPoint._1

        for (r <- field.rocks) { //test if a rock is in the way
          val allX: Vector[Double] = Vector(r.x.toDouble - 0.5, r.x.toDouble + 0.5)
          val allY: Vector[Double] = Vector(r.y.toDouble - 0.5, r.y.toDouble + 0.5)
          for (x <- allX) {
            val y :Double = m * x + c
            if (y >= allY(0) && y <= allY(1)) distance = -1
          }
          for (y <- allY) {
            val x :Double = (y - c) / m
            if (x.isNaN){//checking for rocks if shot is parallel to y-Axis
              if((allX(0) == attPoint._1 || allX(1) == attPoint._1 || r.x == attPoint._1)
                && (Math.signum(r.y - attPoint._2) == Math.signum(defPoint._2 - attPoint._2)
                && Math.abs(r.y - attPoint._2) < Math.abs(defPoint._2 - attPoint._2))){
                distance = -1;
              }
            }
            if (x >= allX(0) && x <= allX(1)) distance = -1
          }
        }

        for (h <- field.character if(h != attHero && h != defHero)) { // test if a character is in the way
          val allX: Vector[Double] = Vector(h.cell.x.toDouble - 0.5, h.cell.x.toDouble + 0.5)
          val allY: Vector[Double] = Vector(h.cell.y.toDouble - 0.5, h.cell.y.toDouble + 0.5)
          for (x <- allX) {
            val y = m * x + c
            if (y >= allY(0) && y <= allY(1)) distance = -1
          }
          for (y <- allY) {
            val x = (y - c) / m
            if (x.isNaN){//checking for characters if shot is parallel to y-Axis
              if((allX(0) == attPoint._1 || allX(1) == attPoint._1 || h.cell.x == attPoint._1)
                && (Math.signum(h.cell.y - attPoint._2) == Math.signum(defPoint._2 - attPoint._2)
                && Math.abs(h.cell.y - attPoint._2) < Math.abs(defPoint._2 - attPoint._2))){
                distance = -1;
              }
            }
            if (x >= allX(0) && x <= allX(1)) distance = -1
          }
        }
        if (distance > 0 && math.ceil(distance) < minDistance) minDistance = math.ceil(distance).toInt//lowest hitting distance
      }
    }

    //special cases
    if (minDistance > attHero.srange) {//shot out of range
      return 0
    }
    //keeping percentage in given range
    val minPercentage = 20
    val maxPercentage = 99
    if (attHero.srange == 1) {
      if (globalDirectionVector._1 == 0 || globalDirectionVector._2 == 0
        || globalDirectionVector._1 == globalDirectionVector._2) return 95
      return 0
    }

    //calculating percentage
    val hitChance = maxPercentage - (((maxPercentage - minPercentage) / (attHero.srange - 1)) * minDistance)
    if (hitChance < 20) {
      return 20
    }
    hitChance
  }

  def out(str: String): Unit = {
    output = str
    publish(new UpdateText)
  }


  def wrongInput(input: String): Unit = {
    out("What are you trying to say with [" + input + "]?")
  }

  def wrongGameState() = out("We should focus on other problems first")

  def fieldToString: String = field.toString

  //return the displaynames of all Characters with their position as Int
  def getCharacters: Vector[(String, Int, Int)] = {
    val temp = new ListBuffer[(String, Int, Int)]()
    for (x <- field.character) {
      temp.append((x.displayname, x.cell.x, x.cell.y))
    }
    temp.toVector
  }

  //return the Team of a Character given their displayname
  def getCharactersSide(hero: String): Int = {
    for (x <- field.character) {
      if (x.displayname == hero) return x.side
    }
    -1
  }

  //used to represent every Charactertype with the right picture in GUI
  def getCharactersTypeIcon(hero: String): String = {
    for (x <- field.character) {
      if (x.displayname == hero) {
        x.name match {
          case "Sniper" => return "sniper"
          case "Tank" => return "tank"
          case "Assassin" => return "shotgun"
          case "Assassin Nr.2" => return "knife"
          case _ => return "rifle"
        }
      }
    }
    ""
  }

  //return the positions of all rocks an Int
  def getRocks: Vector[(Int, Int)] = {
    val temp = new ListBuffer[(Int, Int)]()
    for (x <- field.rocks) {
      temp.append((x.x, x.y))
    }
    temp.toVector
  }

  //Number of Scenarios to choose from
  def scenarioAmmount: Int = {
    val scenario = injector.instance[Scenario]
    scenario.getAmmount()
  }

  //Manipulate and Check characteristics of Strings
  def splitFlatString(input: String): Array[String] = {
    input.replace(',', ' ').split("\\s+")
  }

  def testInt(input: String): Boolean = {
    input.forall(_.isDigit)
  }

  def abcToInt(str: String): Int = {
    val chr = str.charAt(0)
    chr - 'A' + 1
  }

  def testABC(str: String): Boolean = {
    if (str.length == 1) {
      val chr = str.charAt(0)
      if (chr >= 'A' && chr <= 'A' + field.sizeX) {
        return true
      }
    }
    false
  }

  def opponent(hero1: model.Character, hero2: model.Character): Boolean = {
    if (hero1.side == hero2.side) throw new Exception("Are you blind? Why should I shoot my mate?") else true
  }

  def scenarioAmmountTest(input: Try[Int]): Try[Boolean] = Try(input.get >= 0 && input.get <= scenarioAmmount)

  def checkSide(side: Int): Boolean = {
    if (PlayerStatus.turn(PlayerState) == side) true else throw new Exception("Why would I listen to you? You're my enemy")
  }

  def helpOut = publish(new UpdateHelp)

  def infoOut = publish(new UpdateInfo)

  def requestRepaint = publish(new UpdateField)
}
