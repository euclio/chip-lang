package chipLang.ir

/*
 * Song
 * Song represents an entire playable Chiptune song. At its core, it
 * is a list of notes with various modifiers applied to them. A song
 * could be played, or perhaps translated to a written form such as 
 * sheet music.
 */
case class Song(phrases: List[Phrase])

/*
 * Phrase
 * A phrase is collection of verses that can have varying speeds applied to them. 
 * Phrases may be assigned to variables.
 */
sealed abstract class Phrase
case class PhraseAssignment(identifier: String, phrase: PhraseStatement) extends Phrase
case class PhraseIdentifier(name: String) extends Phrase
case class PhraseStatement(bpm: Option[Int], channels: Channels) extends Phrase

/*
 * Channels
 * A Channels class is a collection of verses that are meant to be played at
 * the same time. Channels are usually used for a melody and a bass line, but
 * they are also used for chords.
 */
case class Channels(vs: List[Verse])

/*
 * Verse
 * A verse is a collection of notes that can be modified with an instrument. So,
 * it is possible for users to play the same notes with a different instrument.
 * Verses may also be assigned to variables.
 */
sealed abstract class Verse
case class VerseAssignment(identifier: String, statement: VerseStatement) extends Verse
case class VerseIdentifier(name: String) extends Verse
case class VerseStatement(verses: List[VerseSingleton]) extends Verse
case class VerseSingleton(inst: Option[Instrument], notes: List[Notation])

/*
 *  Instruments
 *  My implementation of Chiptune uses sound waves available on the Nintendo
 *  Entertainment System. On this system there are nine types of wave: four
 *  variations of Square , a Triangle, Sine wave, a white noise track 
 *  (for percussion) and a SquareDown wave for certain sound effects.
 */
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

/*
 * Notation
 * Notation represents a playable sound. In the case of a Note, it has
 * an octave, a pitch, and a duration. As a rest, a notation needs only
 * a duration.
 */
sealed abstract class Notation
sealed trait Octaveless
case class Note(octave: Octave, sound: Sound) extends Notation
case class Sound(pitch: Pitch, accidental: Accidental, dur: Duration) extends Octaveless
case class Rest(duration: Duration) extends Notation with Octaveless

/*
 * Pitch and Octave
 * Pitch represents the letters A-G in musical notation.
 * Octave represents the 8 repetitions of the seven pitches along the major scale
 */
case class Pitch(pitch: Char) { require('A' <= pitch && pitch <= 'G') }
case class Octave(octave: Int) { require(0 <= octave && octave <= 9) }

/*
 * Duration
 * Duration is an object that represents all of the modifications that a user can
 * apply to the duration of a note. A note may be extended or halved. Combinations
 * of these can be used to indicate complex timings like triplets. Dots directly
 * corresponds to a "dotted" note in musical notation, where it means that half
 * of the note's duration should be added to itself.
 */
case class Duration(lengthMods: List[LengthModifier], dots: Int)
sealed abstract class LengthModifier
case object Extend extends LengthModifier
case object Halve extends LengthModifier

/*
 * Accidental
 * The accidentals are raising or lowering of a notes pitch. A sharp is halfway above
 * the current note, and a flat is halfway below. Natural means that there is no change
 * to the notes pitch.
 */
sealed abstract class Accidental
case object Sharp extends Accidental
case object Flat extends Accidental
case object Natural extends Accidental