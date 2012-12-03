import chipLang.syntax._
import scala.io.Source
import chipLang.semantics._

object ChipCompiler extends App {
  require(args.length == 1, "Usage: chiplang [ .cl file ].")

  val program = Source.fromFile(args(0)).mkString

  ChipParser(program) match {
    case ChipParser.Success(ir, _) => {
      println(args(0) + " parsed successfully.")
      println("Compiling...")
      try {
        compile(ir)
      }

      println("Compiled output.midi successfully.")
    }

    case ChipParser.NoSuccess(msg, _) => {
      println("Parsing failed. " + msg)
    }
  }
}