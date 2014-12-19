import scala.io.Source

import chipLang.syntax._
import chipLang.semantics._

object ChipLang extends App {
  require(args.length == 1, "Usage: chiplang [ .cl file ].")

  val fileName = args(0)
  val program = Source.fromFile(fileName).mkString

  println("Compiling '%s'...".format(fileName))
  ChipParser(program) match {
    case ChipParser.Success(ir, _) => {
      // Attempt to compile intermediate representation
      compile(ir, fileName.substring(0, fileName.indexOf('.')))
    }
    case ChipParser.NoSuccess(msg, _) => {
      // Display error returned by the parser
      println("Parsing '%s' failed. ".format(fileName) + msg)
    }
  }
}