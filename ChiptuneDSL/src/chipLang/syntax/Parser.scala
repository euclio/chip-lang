package chipLang.syntax

import scala.util.parsing.combinator._
import chipLang.ir._

object ChipParser extends RegexParsers with PackratParsers {
  def apply(program: String) = parseAll(song, program)

  override def skipWhitespace = false
  val ws = """\s*""".r
  val wsReq = """\s+""".r

  lazy val song: PackratParser[Song] = repsep(phrase, wsReq) ^^ { case ps => Song(ps) }

  def identifier = """[A-Za-z][A-Za-z0-9]*""".r

  def assignment: Parser[String] = identifier <~ ws ~ "="

  lazy val phrase: PackratParser[Phrase] = phraseStatement |
    assignment ~ ws ~ phraseStatement ^^ { case identifier ~ _ ~ phrase => PhraseAssignment(identifier, phrase) } |
    identifier ^^ { PhraseIdentifier(_) }

  def phraseStatement: Parser[PhraseStatement] = repsep(verse, wsReq) ^^ { v => PhraseStatement(None, Channels(v)) } |
    opt(bpm) ~ ((ws ~ "{" ~ ws) ~> channels <~ (ws ~ "}" ~ ws)) ^^ { case opts ~ cs => PhraseStatement(opts, cs) }

  def bpm: Parser[Int] = (ws ~ "(" ~ ws) ~> """\d+""".r <~ (ws ~ ")" ~ ws) ^^ { _.toInt }

  def channels: Parser[Channels] =
    repsep(verse, (ws ~ "&" ~ ws)) ^^ { case vs => Channels(vs) } |
      verse ^^ { case v => Channels(List(v)) }

  lazy val verse: PackratParser[Verse] = assignment ~ ws ~ verseList ^^ { case assignment ~ _ ~ vList => VerseAssignment(assignment, VerseStatement(vList)) } |
    identifier ^^ { VerseIdentifier(_) } |
    verseList ^^ { VerseStatement(_) }

  lazy val verseList: PackratParser[List[VerseSingleton]] =
    verseSingleton ~ (ws ~ "*" ~ ws) ~ """\d+""".r ^^ { case vs ~ _ ~ n => List.fill(n.toInt)(vs) } |
      repsep(verseSingleton, (ws ~ "+" ~ ws)) |
      verseSingleton ^^ { List(_) }

  def verseSingleton: Parser[VerseSingleton] = opt(instrument <~ ws ~ ":") ~ ws ~ notes ^^ { case i ~ _ ~ ns => VerseSingleton(i, ns) }

  def notes: Parser[Notes] =
    octave ~ ws ~ ("[" ~ ws ~> repsep(octaveless, wsReq) <~ ws ~ "]") ^^ { case o ~ _ ~ ols => new Notes(o, ols) } |
      repsep(notation, wsReq) ^^ { case notations => Notes(notations) } |
      notes ~ wsReq ~ notes ^^ { case n1 ~ _ ~ n2 => new Notes(n1, n2) }

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
    "_" ~> opt(duration) ^^ { dur => Rest(dur.getOrElse(Duration(1))) }

  def sound: Parser[Sound] =
    pitch ~ opt(accidental) ~ opt(duration) ^^ {
      case pitch ~ acc ~ dur => Sound(pitch, acc.getOrElse(Natural), dur.getOrElse(Duration(1)))
    }

  def octave: Parser[Octave] =
    "[0-8]".r ^^ { s => Octave(s.toInt) }

  def pitch: Parser[Pitch] =
    "[A-G]".r ^^ { s => Pitch(s.head) }

  def duration: Parser[Duration] =
    rep1("~") ^^ { ts => Duration(ts.length + 1) } |
      rep1("/") ^^ { ds => Duration(1 / Math.pow(2, ds.length)) }

  def accidental: Parser[Accidental] =
    "#" ^^ { _ => Sharp } |
      "b" ^^ { _ => Flat }
}