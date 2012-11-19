package chipLang.syntax

import scala.util.parsing.combinator._
import chipLang.ir._

object ChipParser extends RegexParsers with PackratParsers {
  object defaults {
    val TimeSig = TimeSignature(4, 4)
    val BPM = chipLang.ir.BPM(120)
    val Opts = Options(TimeSig, BPM)
  }

  def apply(program: String) = parseAll(song, program)

  override val skipWhitespace = false
  val ws = """\s+""".r

  def song: Parser[Song] = repsep(phrase, ws) ^^ { case ps => Song(ps) }

  def phrase: Parser[Phrase] = verseList ^^ { v => Phrase(defaults.Opts, Channels(List(v))) } |
    options ~ """\s*{\s*""".r ~ channels ~ """\s*}\s*""".r ^^ { case opts ~ _ ~ cs ~ _ => Phrase(opts, cs) }

  def options: Parser[Options] = opt(timeSig <~ "|") ~ opt(bpm <~ "|") ^^ { case o ~ b => Options(o.getOrElse(defaults.TimeSig), b.getOrElse(defaults.BPM)) }

  def bpm: Parser[BPM] = """\d+""".r ^^ { s => BPM(s.toInt) }

  def timeSig: Parser[TimeSignature] = """4/4|2/2|4/2|2/4|3/4|3/8|6/8""".r ^^ { (i => TimeSignature(i.head, i.charAt(i.size - 1))) }

  def channels: Parser[Channels] =
    repsep(verseList, "&") ^^ { case vs => Channels(vs) } |
      verseList ^^ { case v => Channels(List(v)) }

  def verseList: Parser[VerseList] = verse ^^ { case v => VerseList(List(v)) } |
    repsep(verse, "+") ^^ { case vs => VerseList(vs) } |
    verse ~ "*" ~ """\d+""".r ^^ {
      case v ~ "*" ~ n => {
        val verses = Nil
        for (i <- 0 until n.toInt) v :: verses
        VerseList(verses)
      }
    }

  def verse: Parser[Verse] = opt(instrument <~ ":") ~ notes ^^ { case i ~ ns => Verse(i.getOrElse(Square1), List(ns)) }

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
    pitch ~ opt(accidental) ~ duration ^^ {
      case p ~ a ~ ds => Sound(p, a.getOrElse(Natural), ds)
    }

  def octave: Parser[Octave] =
    "[0-8]".r ^^ { s => Octave(s.toInt) }

  def pitch: Parser[Pitch] =
    "[A-G]".r ^^ { s => Pitch(s.head) }

  def duration: Parser[Duration] =
    rep("~") ^^ { ts => Duration(ts.length + 1) } |
      rep("/") ^^ { ds => Duration(1 / ds.length) }

  def accidental: Parser[Accidental] =
    "#" ^^ { _ => Sharp } |
      "b" ^^ { _ => Flat }
}