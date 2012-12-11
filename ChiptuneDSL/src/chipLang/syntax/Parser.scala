package chipLang.syntax

import scala.util.parsing.combinator._
import chipLang.ir._

object ChipParser extends RegexParsers with PackratParsers {
  def apply(program: String) = parseAll(song, program)

  override def skipWhitespace = false
  val ws = """\s*""".r
  val wsReq = """\s+""".r

  def song: Parser[Song] = repsep(phrase, (ws ~ ";" ~ ws)) ^^ { case ps => Song(ps) }

  def identifier = """[A-Za-z][A-Za-z0-9]*""".r

  def assignment: Parser[String] = identifier <~ ws ~ "="

  lazy val phrase: PackratParser[Phrase] =
    assignment ~ ws ~ phraseStatement ^^ { case identifier ~ _ ~ phrase => PhraseAssignment(identifier, phrase) } |
      identifier ^^ { PhraseIdentifier(_) } |
      phraseStatement

  lazy val phraseStatement: PackratParser[PhraseStatement] =
    opt(bpm) ~ (ws ~ "{" ~ ws) ~ channels <~ (ws ~ "}" ~ ws) ^^ { case opts ~ _ ~ cs => PhraseStatement(opts, cs) } |
      opt(bpm) ~ verse ^^ { case opts ~ v => PhraseStatement(opts, Channels(List(v))) }

  def bpm: Parser[Int] = ("(" ~ ws) ~> """\d+""".r <~ (ws ~ ")") ^^ { _.toInt }

  def channels: Parser[Channels] =
    repsep(verse, (ws ~ "&" ~ ws)) ^^ { case vs => Channels(vs) } |
      verse ^^ { case v => Channels(List(v)) }

  lazy val verse: PackratParser[Verse] =
    assignment ~ ws ~ verseList ^^ { case assignment ~ _ ~ vList => VerseAssignment(assignment, VerseStatement(vList)) } |
      identifier ^^ { VerseIdentifier(_) } |
      verseList ^^ { VerseStatement(_) }

  lazy val verseList: PackratParser[List[VerseSingleton]] =
    verseSingleton ~ (ws ~ "*" ~ ws ~> """\d+""".r) ^^ { case vs ~ n => List.fill(n.toInt)(vs) } |
      repsep(verseSingleton, (ws ~ "+" ~ ws)) |
      verseSingleton ^^ { List(_) }

  def verseSingleton: Parser[VerseSingleton] = opt("|" ~> instrument <~ "|") ~ ws ~ notes ^^ { case i ~ _ ~ ns => VerseSingleton(i, ns) }

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
    "_" ~> duration ^^ { dur => Rest(dur) }

  def sound: Parser[Sound] =
    pitch ~ opt(accidental) ~ duration ^^ {
      case pitch ~ acc ~ dur => Sound(pitch, acc.getOrElse(Natural), dur)
    }

  def octave: Parser[Octave] =
    "[0-8]".r ^^ { s => Octave(s.toInt) }

  def pitch: Parser[Pitch] =
    "[A-G]".r ^^ { s => Pitch(s.head) }

  // duration ::= [ "~" | "/" ], [ "." ];
  def duration: Parser[Duration] =
    rep("~" | "/") ~ rep(".") ^^ {
      case lengthMods ~ dots =>
        val lengthModifiers = lengthMods.map {
          _ match {
            case "~" => Extend
            case "/" => Halve
          }
        }
        Duration(lengthModifiers, dots.length)
    }

  // accidental ::= "#" | "b";
  def accidental: Parser[Accidental] =
    "#" ^^ { _ => Sharp } |
      "b" ^^ { _ => Flat }
}