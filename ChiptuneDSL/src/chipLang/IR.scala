package chipLang.ir

sealed abstract class Identifier
case class PhraseIdentifier extends Identifier
case class VerseIdentifier extends Identifier

case class Song(ps: List[Phrase])

case class Phrase(opts: Options, cs: Channels)

case class Options(ts: TimeSignature, bpm: BPM)
case class BPM(bpm: Int) { require(40 <= bpm && bpm <= 200 && bpm % 10 == 0) }
case class TimeSignature(top: Int, bot: Int)

case class Channels(vs: List[VerseList])

case class VerseList(vs: List[Verse])

case class Verse(inst: Instrument, notes: List[Notes])

case class Notes(ln: List[Notation]) {
  // Constructor to make a Notes from an octave and a list of Octaveless
  def this(o: Octave, nl: List[Octaveless]) = {
    this(for (n <- nl) yield n match {
      case s: Sound => Note(o, s)
      case s: Rest => s
    })
  }

  def this(n1: Notes, n2: Notes) {
    this(n1.ln ++ n2.ln)
  }
  
  def + (ns: Notes) {
    Notes(this.ln ++ ns.ln)
  }
}

sealed abstract class Instrument
case object Square1 extends Instrument
case object Square2 extends Instrument
case object Square3 extends Instrument
case object Square4 extends Instrument
case object Triangle extends Instrument
case object Saw extends Instrument
case object Sine extends Instrument
case object WhiteNoise extends Instrument
case object SquareDown extends Instrument

sealed abstract class Notation
sealed trait Octaveless
case class Note(o: Octave, s: Sound) extends Notation
case class Sound(p: Pitch, a: Accidental, d: Duration) extends Octaveless
case class Rest(d: Duration) extends Notation with Octaveless {
  def this(d: Duration, o: Octave) { this(d) }
}

case class Pitch(p: Char) { require('A' <= p && p <= 'G') }
case class Octave(o: Int) { require(0 <= o && o < 9) }
case class Duration(d: Double)
sealed abstract class Accidental
case object Sharp extends Accidental
case object Flat extends Accidental
case object Natural extends Accidental
  




