package chipLang

import chipLang.ir._
import java.io.File
import javax.sound.sampled.AudioFileFormat
import com.sun.media.sound.SF2Soundbank

package object semantics {
  def eval(s: Song): Unit = for (p <- s.ps) playPhrase(p)

  def playPhrase(p: Phrase): Unit = {
    val options = p.opts

    val SecondsPerMin = 60
    val MillisPerSec = 1000

    val millisPerBeat = (options.bpm.bpm) * SecondsPerMin / MillisPerSec
    val durationPerBeat = 1 / (options.ts.bot / 4)

  }

  def addNote(n: Note, songCounter: Int): Unit = {
    import javax.sound.midi._
    val sequencer = MidiSystem.getSequencer()
    val synth = MidiSystem.getSynthesizer()

    // Set current open transmitter to use the default synth
    val seqTrans = sequencer.getTransmitters().get(0)
    seqTrans.setReceiver(synth.getReceiver())

    synth.open()
    synth.unloadAllInstruments(synth.getDefaultSoundbank())
    synth.loadAllInstruments(new SF2Soundbank(new File("src/resources/Famicom.sf2")))

    sequencer.open()

    synth.getLoadedInstruments().map { i => println(i.getName) }
    val seq = new Sequence(Sequence.PPQ, 10)

    val track = seq.createTrack()

    val pitch = 8 * n.o.o + (n.s.p.p - 40)

    def on(pitch: Int, pressure: Int): ShortMessage =
      new ShortMessage(ShortMessage.NOTE_ON, pitch, pressure)

    def off(pitch: Int, pressure: Int): ShortMessage =
      new ShortMessage(ShortMessage.NOTE_OFF, pitch, pressure)

    track.add(new MidiEvent(on(pitch, 80), 1))
    track.add(new MidiEvent(off(pitch, 80), 20))

    sequencer.setSequence(seq)

    sequencer.start()

    MidiSystem.write(seq, 1, new File("output.midi"))

  }

}