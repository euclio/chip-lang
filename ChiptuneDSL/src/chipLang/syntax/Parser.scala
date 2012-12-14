package chipLang.syntax

import scala.util.parsing.combinator._
import chipLang.ir._

object ChipParser extends RegexParsers with PackratParsers {
  // Syntactic sugar: "ChipParser(program)"
  def apply(program: String) = parseAll(song, program)

  // Ensure that whitespace is disallowed inside notes
  override def skipWhitespace = false

  val ws = """\s*""".r // Optional whitespace
  val wsReq = """\s+""".r // Required whitespace

  // song ::= { phrase, ";" };
  def song: Parser[Song] = rep1sep(phrase, (ws ~ ";" ~ ws)) ^^ { case ps => Song(ps) }

  // identifier ::= ? any sequence of letters or numbers starting with a letter ?;
  def identifier = """[A-Za-z][A-Za-z0-9]*""".r

  // assignment ::= identifier, "=";
  def assignment: Parser[String] = identifier <~ ws ~ "="

  // phrase ::= assignment, phraseStatement | identifier | phraseStatement;
  lazy val phrase: PackratParser[Phrase] =
    assignment ~ ws ~ phraseStatement ^^ { case identifier ~ _ ~ phrase => PhraseAssignment(identifier, phrase) } |
      identifier ^^ { PhraseIdentifier(_) } |
      phraseStatement

  // phraseStatement ::= [ bpm ], ( "{", channels, "}" | verse );
  lazy val phraseStatement: PackratParser[PhraseStatement] =
    opt(bpm) ~ (ws ~ "{" ~ ws) ~ channels <~ (ws ~ "}" ~ ws) ^^ { case opts ~ _ ~ cs => PhraseStatement(opts, cs) } |
      opt(bpm) ~ verse ^^ { case opts ~ v => PhraseStatement(opts, Channels(List(v))) }

  // bpm ::= "(", number, ")";
  def bpm: Parser[Int] = ("(" ~ ws) ~> """\d+""".r <~ (ws ~ ")") ^^ { _.toInt }

  // channels ::= verse, { "&", verse }
  def channels: Parser[Channels] = rep1sep(verse, (ws ~ "&" ~ ws)) ^^ { case vs => Channels(vs) }

  // verse ::= assignment, verseList | identifier | verseList;
  lazy val verse: PackratParser[Verse] =
    assignment ~ ws ~ verseList ^^ { case assignment ~ _ ~ vList => VerseAssignment(assignment, VerseStatement(vList)) } |
      identifier ^^ { VerseIdentifier(_) } |
      verseList ^^ { VerseStatement(_) }

  // verseList ::= verseSingleton, "*", number | { verseSingleton, "+" };
  lazy val verseList: PackratParser[List[VerseSingleton]] =
    verseSingleton ~ (ws ~ "*" ~ ws ~> """\d+""".r) ^^ { case vs ~ n => List.fill(n.toInt)(vs) } |
      repsep(verseSingleton, (ws ~ "+" ~ ws))

  // verseSingleton ::= [ "|", instrument, "|" ], notes;
  def verseSingleton: Parser[VerseSingleton] = opt("|" ~> instrument <~ "|") ~ ws ~ notes ^^ { case i ~ _ ~ ns => VerseSingleton(i, ns) }

  // notes ::= octave, "[", octaveless, { ws, octaveless }, "]" | notation, { notation, ws } | notes, { ws, notes };
  lazy val notes: PackratParser[List[Notation]] =
    rep1sep(notes, wsReq) ^^ { _.flatten } |
      octave ~ ws ~ ("[" ~ ws ~> rep1sep(octaveless, wsReq) <~ ws ~ "]") ^^ {
        case octave ~ _ ~ octavelessList => for (octaveless <- octavelessList) yield octaveless match {
          case s: Sound => Note(octave, s)
          case r: Rest => r
        }
      } | rep1sep(notation, wsReq)

  // instrument ::= "SQ1" | "SQ2" | "SQ3" | "SQ4" | "SAW" | "TRI" | "SIN" | "WHI" | "SQD";
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

  // notation ::= octave, sound;
  def notation: Parser[Notation] =
    octave ~ sound ^^ { case o ~ s => Note(o, s) } | rest

  // octaveless ::= sound | rest;
  def octaveless: Parser[Octaveless] = sound | rest

  // rest ::= "_", duration;
  def rest: Parser[Rest] =
    "_" ~> duration ^^ { dur => Rest(dur) }

  // sound ::= pitch, [ accidental ], duration;
  def sound: Parser[Sound] =
    pitch ~ opt(accidental) ~ duration ^^ {
      case pitch ~ acc ~ dur => Sound(pitch, acc.getOrElse(Natural), dur)
    }

  // octave ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
  def octave: Parser[Octave] =
    "[0-9]".r ^^ { s => Octave(s.toInt) }

  // pitch ::= "A" | "B" | "C" | "D" | "E" | "F" | "G";
  def pitch: Parser[Pitch] =
    "[A-G]".r ^^ { s => Pitch(s.head) }

  // duration ::= { "~" | "/" }, { "." };
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