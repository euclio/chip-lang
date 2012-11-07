package chipLang.syntax

import scala.util.parsing.combinator._
import chipLang.ir._

class ChipParser extends RegexParsers with PackratParsers {
  object defaults {
    val TimeSig = TimeSignature(4, 4)
    val BPM = chipLang.ir.BPM(120)
    val Opts = Options(TimeSig, BPM)
  }

  def apply(s: String) = parseAll(song, s)
  override val skipWhitespace = false
  val ws = """\s*""".r

  def song: Parser[Song] = rep(phrase) ^^ { Song(_) }

  def phrase = opt(options) ~ "{" ~ channels ~ "}" ^^ { case opts ~ "{" ~ cs ~ "}" => Phrase(opts.getOrElse(defaults.Opts), cs) } |
    verse ^^ { v => Phrase(defaults.Opts, Channels(List(v))) }

  def options = opt(timeSig) ~ opt("|" ~> bpm) <~ "|" ^^ { case o ~ b => Options(o.getOrElse(defaults.TimeSig), b.getOrElse(defaults.BPM)) }

  def bpm: Parser[BPM] = """\d+""".r ^^ { s => BPM(s.toInt) }

  def timeSig: Parser[TimeSignature] = """4/4|2/2|4/2|2/4|3/4|3/8|6/8""".r ^^ { (i => TimeSignature(i.head, i.charAt(i.size - 1))) }

  def channels: Parser[Channels] =
    verse ~ opt(ws) ~ "&" ~ opt(ws) ~ channels ^^ { case v ~ _ ~ "&" ~ _ ~ c => Channels(v :: c.vs) } |
      verse ^^ { case v => Channels(List(v)) } |
      repsep(verse, "&") ^^ { Channels(_) }

  lazy val verse: PackratParser[Verse] =
    opt(instrument <~ ":") ~ notes ^^ { case i ~ ns => Verse(i.getOrElse(Square1), List(ns)) }

  lazy val notes: PackratParser[Notes] =
    octave ~ "[" ~ rep(octaveless) ~ "]" ^^ { case o ~ "[" ~ ols ~ "]" => new Notes(o, ols) } |
      notation ~ rep(notation) ^^ { case n ~ ns => Notes(n :: ns) } |
      notes ~ notes ^^ { case n1 ~ n2 => new Notes(n1, n2) }

  def instrument: Parser[Instrument] =
    """SQ1|SQ2|SQ3|SQ4|SAW|TRI|SIN|WHI|SQD""".r ^^ {
      _ match {
        case "SQ1" => Square1
        case "SQ2" => Square2
        case "SQ3" => Square3
        case "SQ4" => Square4
        case "SAW" => Saw
        case "TRI" => Triangle
        case "SIN" => Sine
        case "WHI" => WhiteNoise
        case "SQD" => SquareDown
      }
    }

  def notation: Parser[Notation] =
    octave ~ sound ^^ { case o ~ s => Note(o, s) } | rest

  def octaveless: Parser[Octaveless] = sound | rest

  def rest: Parser[Rest] =
    "_" ~> rep("~") ^^ { l => Rest(Duration(l.size + 1)) }

  def sound: Parser[Sound] =
    pitch ~ opt(accidental) ~ rep("~") ^^ { case p ~ a ~ ds => Sound(p, a.getOrElse(Natural), Duration(ds.size + 1)) }
  pitch ~ opt(accidental) ~ "/" ~ """2|4|8""".r ^^ { case p ~ a ~ "/" ~ d => Sound(p, a.getOrElse(Natural), Duration(1 / d.toDouble)) }

  def octave: Parser[Octave] =
    "[0-8]".r ^^ { s => Octave(s.toInt) }

  def pitch: Parser[Pitch] =
    "[A-G]".r ^^ { s => Pitch(s.head) }

  def duration: Parser[Duration] =
    """1|2|4|8""".r ^^ { s => Duration(s.toDouble) } |
      "1" ~ "/" ~> """2|4|8""".r ^^ { case d => Duration(1 / d.toDouble) }

  def accidental: Parser[Accidental] =
    "#" ^^ { _ => Sharp } |
      "b" ^^ { _ => Flat }
}

object Main extends App {
  print(new ChipParser().apply("1A#~~~"))
}
