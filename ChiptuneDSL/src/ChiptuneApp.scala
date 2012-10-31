import javax.sound.midi._
import com.sun.media.sound.SF2Soundbank
import java.io.File

trait ChipLang {
  // Soundbank constants
  private val FAMICOM = new File("Famicom.sf2")
  private val GAMEBOY = new File("gbfont.sf2")
  private val MIDI8BIT = new File("GXSCC_gm_033.sf2")

  private val soundbank = new SF2Soundbank(FAMICOM)

  // Set up synthesizer for chiptune
  val synth = MidiSystem.getSynthesizer
  synth.open()
  synth.unloadAllInstruments(synth.getDefaultSoundbank)
  synth.loadAllInstruments(soundbank)

  val instruments = soundbank.getInstruments()

  val channels = synth.getChannels
  
  

  val SQ1 = instruments(3) // Square Wave 12%
  val SQ2 = instruments(2) // Square Wave 25%
  val SQ3 = instruments(1) // Square Wave 38%
  val SQ4 = instruments(0) // Square Wave 50%
  val SAW = instruments(4) // Saw Wave
  val TRI = instruments(5) // Triangle Wave
  val SIN = instruments(6) // Sine Wave
  val WHI = instruments(7) // White Noise
  val SQD = instruments(8) // Square Down
}

trait Chiptune extends ChipLang

object Main2 extends Chiptune {
  for (i <- synth.getLoadedInstruments) {
    println(i.getName)
  }

  val leadPatch = instruments(1).getPatch

  channels(4).programChange(leadPatch.getBank, leadPatch.getProgram)

  // Play music
  for (i <- synth.getLoadedInstruments) {
    val patch = i.getPatch
    channels(4).programChange(patch.getBank, patch.getProgram)
    channels(4).noteOn(60, 80)
    Thread.sleep(500)
    channels(4).noteOff(60)
    Thread.sleep(300)
  }

  synth.close();
}