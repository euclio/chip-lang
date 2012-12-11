package chipLang.ir

case class Song(phrases: List[Phrase])

sealed abstract class Phrase
case class PhraseAssignment(identifier: String, phrase: PhraseStatement) extends Phrase
case class PhraseIdentifier(name: String) extends Phrase
case class PhraseStatement(bpm: Option[Int], channels: Channels) extends Phrase

case class Channels(vs: List[Verse])

sealed abstract class Verse
case class VerseAssignment(identifier: String, statement: VerseStatement) extends Verse
case class VerseIdentifier(name: String) extends Verse
case class VerseStatement(verses: List[VerseSingleton]) extends Verse
case class VerseSingleton(inst: Option[Instrument], notes: Notes)

case class Notes(noteList: List[Notation]) {
  // Constructor to make a Notes from an octave and a list of Octaveless
  def this(o: Octave, nl: List[Octaveless]) = {
    this(for (n <- nl) yield n match {
      case s: Sound => Note(o, s)
      case s: Rest => s
    })
  }

  def this(n1: Notes, n2: Notes) {
    this(n1.noteList ++ n2.noteList)
  }

  def +(ns: Notes) {
    Notes(this.noteList ++ ns.noteList)
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
case class Note(octave: Octave, sound: Sound) extends Notation
case class Sound(pitch: Pitch, accidental: Accidental, dur: Duration) extends Octaveless
case class Rest(duration: Duration) extends Notation with Octaveless {
  def this(duration: Duration, o: Octave) { this(duration) }
}

case class Pitch(pitch: Char) { require('A' <= pitch && pitch <= 'G') }
case class Octave(octave: Int) { require(0 <= octave && octave < 9) }

case class Duration(lengthMods: List[LengthModifier], dots: Int)
sealed abstract class LengthModifier
case object Extend extends LengthModifier
case object Halve extends LengthModifier

sealed abstract class Accidental
case object Sharp extends Accidental
case object Flat extends Accidental
case object Natural extends Accidental