package chipLang.ir

object constants {
  val DefaultTimeSig = TimeSignature(4, 4)
  val DefaultBPM = BPM(160)
  val DefaultOptions = Options(DefaultTimeSig, DefaultBPM)
}

sealed abstract class Identifier
case class PhraseIdentifier extends Identifier
case class VerseIdentifier extends Identifier

case class Song(ps: List[Phrase])

case class Phrase(opts: Options = constants.DefaultOptions, ps: List[Verse])

case class Options(ts: TimeSignature, bpm: BPM)
case class BPM(bpm: Int) { require(40 <= bpm && bpm <= 200 && bpm % 10 == 0) }
case class TimeSignature(top: Int, bot: Int)

case class Verse(inst: Instrument, channels: List[List[Notation]])

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
case class Rest(d: Duration) extends Notation
case class Sound(o: Octave, p: Pitch, a: Accidental = Natural, d: Duration = Duration(1)) extends Notation

case class Pitch(p: Char) { require('A' <= p && p <= 'G') }
case class Octave(o: Int) { require(0 <= o && o < 9) }
case class Duration(d: Double)
sealed abstract class Accidental
case object Sharp extends Accidental
case object Flat extends Accidental
case object Natural extends Accidental
  




