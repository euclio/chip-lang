package chipLang

import chipLang.ir._
import java.io.FileInputStream

package object semantics {
  val SecsPerMin = 60
  val MillisPerSec = 1000
  val PPQ = 960
  // val DefaultOptions = Options(TimeSignature(4, 4), BPM(120))
  val DefaultInstrument = Square1
  val PatchMap: Map[Instrument, Tuple2[Int, Int]] = Map(Square1 -> (0, 0),
    Square2 -> (0, 1),
    Square3 -> (0, 2),
    Square4 -> (0, 3),
    Saw -> (0, 4),
    Triangle -> (0, 5),
    Sine -> (0, 6),
    WhiteNoise -> (0, 7),
    SquareDown -> (0, 8))

  /* 
   * Class to encapsulate the state of the current song. It handles the
   * stores for Verse and Phrase variables
   */
  object SongState {
    // Maps to store and retrieve variables
    import scala.collection.mutable
    val verseStore = mutable.Map.empty[String, VerseStatement]
    val phraseStore = mutable.Map.empty[String, PhraseStatement]
  }

  /* 
   * Object that represents the current Song to be compiled. This object
   * wraps MIDI messages with less verbose methods. It also internally
   * keeps track of the MIDI program counter to place messages in the
   * correct locations. It handles setup and teardown of the MIDI
   *  libraries and the final .midi output.
   */
  object Song {
    import javax.sound.midi._
    import com.sun.media.sound.SF2Soundbank
    import java.io.File

    // Initialize Chiptune SoundBank
    val soundbank = new SF2Soundbank(new File("src/resources/Famicom.sf2"))

    // Retrieve a sequencer that is not connected to the default MIDI player
    val sequencer = MidiSystem.getSequencer(false)
    sequencer.open()

    // Unload default instruments and load Chiptune instruments on Synthesizer
    val synth = MidiSystem.getSynthesizer()
    synth.open()
    synth.unloadAllInstruments(synth.getDefaultSoundbank())
    synth.loadAllInstruments(soundbank)

    // Connect sequencer output to synthesizer input
    sequencer.getTransmitter().setReceiver(synth.getReceiver())

    // Program counter
    var pc = PPQ

    // Sequence to represent the song itself
    val seq = new Sequence(Sequence.PPQ, PPQ)
    val track = seq.createTrack()

    def changeInstrument(i: Option[ir.Instrument]) {
      i match {
        case None => return
      }

      PatchMap.get(i.get) match {
        case prog: Some[Tuple2[Int, Int]] => track.add(new MidiEvent(programChange(prog.get), pc))
        case None => throw new IllegalArgumentException("Error: Invalid instrument encountered.")
      }
    }

    def addNote(pitch: Int, duration: Double) {
      println(duration)
      val tick = Math.round(duration * PPQ)

      track.add(new MidiEvent(on(pitch, 80), pc))
      track.add(new MidiEvent(off(pitch, 80), pc + tick))

      pc += tick.toInt
    }

    def addRest(duration: Double) {
      val tick = Math.round(duration * PPQ)
      pc += tick.toInt
    }

    private def on(pitch: Int, pressure: Int): ShortMessage =
      new ShortMessage(ShortMessage.NOTE_ON, pitch, pressure)

    private def off(pitch: Int, pressure: Int): ShortMessage =
      new ShortMessage(ShortMessage.NOTE_OFF, pitch, pressure)

    private def programChange(program: (Int, Int)) =
      new ShortMessage(ShortMessage.PROGRAM_CHANGE, program._1, program._2)

    def tick() {
      pc += PPQ
    }

    def write() {
      tick()
      MidiSystem.write(seq, 1, new File("output.midi"))

    }

    def play() {
      sequencer.setSequence(seq)
      sequencer.start()
    }
  }

  // The "program counter", or the variable that keeps track of where we are in the song
  var pc = PPQ

  def compile(s: Song) {
    println(s)
    for (p <- s.phrases) addPhrase(p)
    Song.write()
    Song.play()
  }

  def addPhrase(p: Phrase): Unit = {
    // Determine if the given Phrase is an Assignment, Statement, or Identifier
    val phrase: PhraseStatement = p match {
      case i: PhraseIdentifier => {
        SongState.phraseStore.get(i.name) match {
          case p: Some[PhraseStatement] => p.get
          case None => throw new NoSuchFieldException("Error: phrase identifier {0} not bound.".format(i.name))
        }
      }
      case s: PhraseStatement => s
      case a: PhraseAssignment => {
        // If the phrase is an assignment, add the value to the store
        SongState.phraseStore += a.identifier -> a.phrase
        a.phrase
      }
    }

    //    SongState.setOptions(phrase.bpm)
    playChannel(phrase.channels)
  }

  def playChannel(c: Channels) {
    for (v <- c.vs) {
      playVerse(v)
    }
  }

  def playVerse(v: Verse) {
    // Determine if the given Verse is an Assignment, Statement, or Identifier
    val verses: List[VerseSingleton] = v match {
      case i: VerseIdentifier => {
        SongState.verseStore.get(i.name) match {
          case v: Some[VerseStatement] => v.get.verses
          case None => throw new NoSuchFieldException("Error: verse identifier '%s' not bound.".format(i.name))
        }
      }
      case s: VerseStatement => s.verses
      case a: VerseAssignment => {
        // If the phrase is an assignment, add the value to the store
        SongState.verseStore += a.identifier -> a.statement
        println("Added %s to map.".format(a.identifier))
        a.statement.verses
      }
    }

    for (s <- verses) {
      Song.changeInstrument(s.inst)
      addNotes(s.notes)
    }
  }

  def addNotes(notes: Notes) {
    for (n <- notes.noteList) {
      n match {
        case Note(Octave(octave), Sound(Pitch(pitch), accidental, Duration(duration))) => {
          val accNum = accidental match {
            case Sharp => 1
            case Flat => -1
            case Natural => 0
          }

          val pitchNum = pitch match {
            case 'C' => 0
            case 'D' => 2
            case 'E' => 4
            case 'F' => 5
            case 'G' => 7
            case 'A' => 9
            case 'B' => 11
          }

          val midiNumber = 12 * octave + pitchNum + accNum

          Song.addNote(midiNumber, duration)
        }

        case Rest(Duration(duration)) => {
          Song.addRest(duration)
        }
      }
    }
  }
}

object Test extends App {
  val s = semantics.Song.addNote(1, 3.2)
}