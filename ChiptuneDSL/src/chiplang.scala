import chipLang.syntax._
import scala.io.Source
import chipLang.semantics._

object ChipCompiler extends App {
  require(args.length == 1, "Too many arguments.")
  
  val program = Source.fromFile(args(0)).mkString
  
  ChipParser(program) match {
    case ChipParser.Success(ir, _) => {
      println("Parsed successfully. Compiling...")
      eval(ir)
    }
    
    case ChipParser.NoSuccess(msg, _) => {
      println("Parsing failed. " + msg)
    }
  }
}