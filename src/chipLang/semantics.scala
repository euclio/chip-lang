package chipLang

import chipLang.ir._

package object semantics {
  /* 
   * Encapsulates the state of the current song. It handles the
   * stores for Verse and Phrase variables
   */
  object State {
    // Maps to store and retrieve variables
    import scala.collection.mutable
    val verseStore = mutable.Map.empty[String, VerseStatement]
    val phraseStore = mutable.Map.empty[String, PhraseStatement]
  }

  /* 
   * Represents the current Song to be compiled. This object
   * wraps MIDI messages with less verbose methods. It also internally
   * keeps track of the MIDI program counter to place messages in the
   * correct locations. It handles setup and teardown of the MIDI
   *  libraries and the final .midi output.
   */
  object Song {
    import javax.sound.midi._
    import com.sun.media.sound.SF2Soundbank
    import java.io.File

    // Pulses Per Quarter note, essentially the resolution of the MIDI file
    val PPQ = 960

    // Map that takes Instruments to their Patch numbers
    val PatchMap: Map[chipLang.ir.Instrument, Int] = Map(Square1 -> 0,
      Square2 -> 1,
      Square3 -> 2,
      Square4 -> 3,
      Saw -> 4,
      Triangle -> 5,
      Sine -> 6,
      WhiteNoise -> 7,
      SquareDown -> 8)

    // Initialize Chiptune SoundBank
    val soundbank = new SF2Soundbank(new File("resources/Famicom.sf2"))

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

    // Program counter to keep track of current position in Song
    var pc: Long = PPQ

    // Holds the greatest value that the program counter has been so far
    var greatestPC = pc

    // Create Sequence to represent the song
    val seq = new Sequence(Sequence.PPQ, PPQ)
    val track = seq.createTrack()

    /*
     *  Wrapper method to add a Tempo change event to the Song
     */
    def setSpeed(bpm: Int) {
      // Conversion from bpm to milliseconds per quarter note
      val tempoInMPQ = 60000000 / bpm
      
      // Bit-shifting to get the data for the tempo change MIDI message
      val data = Array((tempoInMPQ >> 16) & 0xFF,
        (tempoInMPQ >> 8) & 0xFF,
        tempoInMPQ & 0xFF)
        .map(_.toByte)
      val changeSpeed = new MetaMessage(0x51, data, data.length)

      addEvent(changeSpeed)
    }

    /*
     * Helper to cut down on the syntax of adding a MidiEvent to
     * the Song's sequence.
     */
    private def addEvent(message: MidiMessage) {
      track.add(new MidiEvent(message, pc))
    }

    /*
     *  Wrapper to add a Program change event to the Song
     */
    def changeInstrument(channel: Int, i: chipLang.ir.Instrument) {
      // Determine the patch number of the given Instrument
      val patch: Int = PatchMap.get(i) match {
        case prog: Some[Int] => prog.get
        case None => throw new IllegalArgumentException("Error: Invalid instrument encountered.")
      }

      // Add the instrument change to the Song
      val programChange = new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, patch, 0)
      addEvent(programChange)
    }

    /*
     *  Wrapper to add a Note On and Note Off event to the song
     *  at the appropriate times relative to the supplied
     *  duration.
     */
    def addNote(pitch: Int, duration: Double, channel: Int) {
      // MIDI only supports 16 channels
      if (channel > 16) throw new IndexOutOfBoundsException("Can only play 16 simultaneous channels")
      
      // Note volume
      val Pressure = 80

      // The MIDI messages to be added to the song.
      val noteOn = new ShortMessage(ShortMessage.NOTE_ON, channel, pitch, Pressure)
      val noteOff = new ShortMessage(ShortMessage.NOTE_OFF, channel, pitch, Pressure)

      // Calculate the length of the tick between NoteOn and NoteOff
      val durationInPPQ = scala.math.round(duration * PPQ)

      // Add the appropriate messages to the Song
      addEvent(noteOn)
      tick(durationInPPQ)
      addEvent(noteOff)
    }

    /*
     * Wrapper method to tick the program counter the appropriate
     * amount to simulate a rest.
     */
    def addRest(duration: Double, channel: Int) {
      val durationInPPQ = scala.math.round(duration * PPQ)
      tick(durationInPPQ)
    }

    /*
     * Ticks the MIDI program counter the given amount
     */
    def tick(amount: Long) {
      pc += amount
      greatestPC = scala.math.max(pc, greatestPC)
    }

    /*
     * Writes the Song's current sequence to a MIDI file using MIDI format '1'.
     */
    def write(fileName: String) {
      tick(PPQ)
      addEvent(new ShortMessage()) // Ensure that the song is not cut off

      MidiSystem.write(seq, 1, new File(fileName + ".midi"))
    }

    /*
     * Method to play the Song's current sequence
     */
    def play() {
      sequencer.setSequence(seq)
      sequencer.start()
      while (sequencer.isRunning()) {
        // Do nothing, program fails to quit without pause
        Thread.sleep(100)
      }
      sequencer.close()
    }

    /*
     * Allow the user to reset the program counter to a given value
     */
    def setProgramCounter(pc: Long) {
      this.pc = pc
    }
  }

  def compile(s: Song, outputFilename: String) {
    // Play each phrase
    for (p <- s.phrases)
      addPhrase(p)

    Song.write(outputFilename)
    println("Compilation to '%s.midi' successful.".format(outputFilename))
    println("Playing chiptune...")
    Song.play()
  }

  def addPhrase(p: Phrase): Unit = {
    // Determine if the given Phrase is an Assignment, Statement, or Identifier
    val phrase: PhraseStatement = p match {
      // If it's an identifier, find the name in the store and retrieve its value
      case i: PhraseIdentifier => {
        State.phraseStore.get(i.name) match {
          case p: Some[PhraseStatement] => p.get
          case None => throw new NoSuchFieldException("Error: phrase identifier '%s' not bound.".format(i.name))
        }
      }
      // If it's a statement, just use the statement
      case s: PhraseStatement => s
      // If it's an assignment, add the value to the store and use the statement
      case a: PhraseAssignment => {
        State.phraseStore += a.identifier -> a.phrase
        a.phrase
      }
    }

    // If there's a BPM, then change the Song's speed
    phrase.bpm match {
      case bpm: Some[Int] => Song.setSpeed(bpm.get)
      case None => ()
    }

    playChannel(phrase.channels)
  }

  def playChannel(c: Channels) {
    // Default channel number
    var channel = 1

    // Keep track of the old program counter
    val oldPC = Song.pc

    for (v <- c.vs) {
      // Return to the original program counter so new notes will overlap
      Song.setProgramCounter(oldPC)
      playVerse(v, channel)

      // Increment channel so notes in a different verse 
      // will go on a different channel

      channel += 1
    }

    // Set the program counter to the greatest program counter value so far.
    // This essentially adds rests to shorter verses so that the verses line up.
    Song.setProgramCounter(Song.greatestPC)
  }

  def playVerse(v: Verse, channel: Int) {
    // Determine if the given Verse is an Assignment, Statement, or Identifier
    val verses: List[VerseSingleton] = v match {
      // If it's an identifier, find the name in the store and retrieve its value
      case i: VerseIdentifier => {
        State.verseStore.get(i.name) match {
          case v: Some[VerseStatement] => v.get.verses
          case None => throw new NoSuchFieldException("Error: verse identifier '%s' not bound.".format(i.name))
        }
      }
      // If it's a statement, just use the statement's verses
      case s: VerseStatement => s.verses
      // If the phrase is an assignment, add the value to the store and use the verses
      case a: VerseAssignment => {
        State.verseStore += a.identifier -> a.statement
        a.statement.verses
      }
    }

    for (s <- verses) {
      // If an instrument is encountered, then change the instrument
      s.inst match {
        case i: Some[Instrument] => Song.changeInstrument(channel, i.get)
        case None => Unit
      }

      addNotes(s.notes, channel)
    }
  }

  def addNotes(notes: List[Notation], channel: Int) {
    for (n <- notes) {
      n match {
        case Note(Octave(octave), Sound(Pitch(pitch), accidental, Duration(modifiers, numDots))) => {
          // Increment if sharp, decrement if flat, do nothing if natural
          val accNum = accidental match {
            case Sharp => 1
            case Flat => -1
            case Natural => 0
          }

          // Each letter has an accidental between it except B/C and E/F
          val pitchNum = pitch match {
            case 'C' => 0
            case 'D' => 2
            case 'E' => 4
            case 'F' => 5
            case 'G' => 7
            case 'A' => 9
            case 'B' => 11
          }

          val duration = calculateDuration(modifiers, numDots)

          // Calculate the number representing the given note
          val midiNumber = 12 * octave + pitchNum + accNum

          Song.addNote(midiNumber, duration, channel)
        }

        case Rest(Duration(modifiers, numDots)) => {
          val duration = calculateDuration(modifiers, numDots)
          Song.addRest(duration, channel)
        }
      }
    }
  }

  /*
   * Takes a list of modifiers and number of dots and calculates the correct
   * duration for a note. Uses the rules of musical notation to compute the
   * correct value.
   */
  def calculateDuration(modifiers: List[LengthModifier], numDots: Int) = {
    // Default duration is 1.0
    var duration = 1.0

    // Halve or extend the duration based on the encountered modifiers
    for (m <- modifiers) {
      m match {
        case Extend => duration += 1
        case Halve => duration /= 2
      }
    }

    // Apply the formula for dots for each dot
    for (i <- 0 until numDots) {
      duration += duration - duration / scala.math.pow(2, i + 1)
    }

    duration
  }
}