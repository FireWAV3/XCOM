package XCOM.model

import org.scalatest.Matchers._
import org.scalatest.WordSpec
import XCOM.model.FieldStructure._

class CharacterSpec extends WordSpec{
  var testCharacter = Character("Tank", 5, 10, 70, 40, 0,"C2", Cell(4, 8, C))

  "A Character" should{
    "have a name" in{
      testCharacter.name should be ("Tank")
    }
    "have a movement range" in{
      testCharacter.mrange should be (5)
    }
    "have a shooting range" in{
      testCharacter.srange should be (10)
    }
    "have damage" in{
      testCharacter.damage should be (70)
    }
    "have Health points" in{
      testCharacter.hp should be (40)
    }
    "have a team" in{
      testCharacter.side should be (0)
    }
    "have a displayname" in{
      testCharacter.displayname should be ("C2")
    }
    "have a X location" in{
      testCharacter.cell.x should be (4)
    }
    "have a Y location" in{
      testCharacter.cell.y should be (8)
    }
    "have a Celltype" in{
      testCharacter.cell.otype should be (C)
    }
  }
}
