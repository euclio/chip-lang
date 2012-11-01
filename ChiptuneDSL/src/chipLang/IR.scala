package chipLang.ir

package object ir {
  val DEFAULT_TIME_SIG = TimeSignature(4, 4)
  val DEFAULT_BPM = BPM(160)
}

case class Song(ps: List[Verse])

case class Verse(ts: TimeSignature = ir.DEFAULT_TIME_SIG, bpm: BPM = ir.DEFAULT_BPM, i: Instrument = Square1, ps: List[Phrase])

abstract class Instrument
object Square1 extends Instrument
object Square2 extends Instrument
object Square3 extends Instrument
object Square4 extends Instrument
object Triangle extends Instrument
object Saw extends Instrument
object Sine extends Instrument
object WhiteNoise extends Instrument
object SquareDown extends Instrument

case class Phrase(ns: List[Notation])

case class TimeSignature(top: Int, bot: Int)
case class BPM(bpm: Int)

abstract class Notation
case class Rest(d: Duration) extends Notation
case class Sound(o: Octave, p: Pitch, i: Accidental = Natural, d: Duration = Duration(1)) extends Notation

case class Pitch(p: Char) { require('A' <= p && p <= 'G') }
case class Octave(o: Int) { require(0 <= o && o < 9) }
case class Duration(d: Int)

sealed abstract class Accidental
case object Sharp extends Accidental
case object Flat extends Accidental
case object Natural extends Accidental


