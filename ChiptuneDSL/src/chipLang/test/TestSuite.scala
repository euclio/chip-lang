package chipLang.test

import chipLang.ir._
import chipLang.syntax._
import org.scalatest._

class TestSuite extends Suites(
    new NoteTests() )

class NoteTests extends FunSpec with ChipFunctions{
  describe("A note") {
    it("needs an octave and a pitch") {
      parser("1A")
    }
  }
}

trait ChipFunctions {
  def parser(program: String) = ChipParser(program)
}