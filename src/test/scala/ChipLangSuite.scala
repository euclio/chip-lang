package chipLang.test

import org.scalatest._
import org.scalatest.Matchers
import chipLang.ir._
import chipLang.syntax._
import chipLang.semantics._

class TestSuite extends Suites(
  new NoteTests(), new VerseTests(), new PhraseTests(), new SongTests()) {
}

class NoteTests extends FunSpec with ChipFunctions {
  describe("A note") {
    it("has an octave between 0 and 8") {
      parsing("0A") should equal(success)
      parsing("8A") should equal(success)
      parsing("10A") should equal(failure)
      parsing("-1C") should equal(failure)
    }

    it("has a pitch between A and G") {
      parsing("5A") should equal(success)
      parsing("5E") should equal(success)
      parsing("5H") should equal(failure)
    }

    it("can have a variable duration") {
      parsing("5C~~~") should equal(success)
      parsing("5C///") should equal(success)
      parsing("5C~/.") should equal(success)
      parsing("5C./~") should equal(failure)
    }
  }
  
  describe("A rest") {
    it("can stand alone") {
      parsing("_") should equal(success)
    } 
    
    it("can have a duration") {
      parsing("_~") should equal(success)
      parsing("_/") should equal(success)
      parsing("_~/.") should equal(success)
      parsing("_.~") should equal(failure)
    }
  }
}

class VerseTests extends FunSpec with ChipFunctions {
  describe("a verse") {
    it("is a list of notes") {
      parsing("0A 1B 2C 3D 4E 5F 6G") should equal(success)
    }

    it("has a shorthand for notes of the same octave") {
      parsing("5[A B C D E F G]") should equal(success)
    }
    
    it("can have arithmetic applied to it") {
      parsing("3A + 2C") should equal(success)
      parsing("4A * 6") should equal(success)
    }
    
    it("can be assigned to a variable") {
      parsing("chorus = 5A 6C 3B") should equal(success)
      parsing("verse = 6C * 4") should equal(success)
    }
    
    it("can have an instrument applied to it") {
      parsing("|SQ1| 5C 4C 3B") should equal(success)
    }
  }
}

class PhraseTests extends FunSpec with ChipFunctions {
  describe("a phrase") {
    it("can have multiple channels") {
      parsing("{|SQ1|  5C~~~~ &\n|SQ2| _ 5E~~~ &\n|SQ3| _~ 5G~~ &\n|SQ4| _~~ 6C~ };") should equal(success)
    }
    
    it("can change the speed") {
      parsing("(120) 5 [ C C C C ];\n(40) 5 [ C C C C ];") should equal (success)
    }
    
    it("can do arithmetic)") {
      parsing("{ 5 [ C D E ] & 4D }; { 6C };") should equal (success)
    }
  }
}

class SongTests extends FunSpec with ChipFunctions {
    describe("Percussion") {
      it("has multiple tracks playing at the same time") {
        compile("percussion.cl")
      }
    }
    
    describe("Prelude") {
      it("is the opening song of Final Fantasy") {
        compile("prelude.cl")
      }
    }
    
    describe("Crystal Tower") {
      it("is the final dungeon of Final Fantasy III (JPN)") {
        compile("crystalTower.cl")
      }
    }
    
    describe("Super Mario Bros. Theme") {
      it("is a classic!") {
        compile("marioBros.cl")
      }
    }
}

trait ChipFunctions extends Matchers {
  def success = true
  def failure = false
  
  def compile(file: String) {
    import scala.io.Source  
    ChipParser(file)
  }
  
  def parsing(program: String) = ChipParser(program) match {
    case ChipParser.Success(_, _) => true
    case ChipParser.NoSuccess(_, _) => false
  }
}
