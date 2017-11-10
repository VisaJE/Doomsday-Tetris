
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1

import scala.util._
import o1.util.tryForURL
import javax.sound.midi._
import sun.util.logging.PlatformLogger._
import javax.sound.sampled._

package object sound {

  
  /* "cdefgah"                plays seven notes at the default tempo of 120. (Note: the seventh note is h, not b.)
   * "CDEFGAH"                plays them louder.
   * "CDEFGAH/240"            plays them at a double tempo of 240.  
   * "CD E"                   has a pause between the second and third note.  
   * "CD-E---"                has a longer second note and a longer still third note.
   * "C.D.E"                  produces a staccato-like effect on the first two notes (playing them shorter followed by a pause).
   * ">CDE<<<CDE"             plays three notes in a higher octave then shifts three octaves down before playing them again.  
   * "C7D3E"                  plays the c in Octave #7, the d in Octave #3, and the e in the default Octave #5.    
   * "CbDBE#7F"               has a c-flat, a d-flat, an e-sharp in Octave #7, and a natural f. b and B are equivalent.
   * "C♭D♭E♯7F♮"              is a fancy-pants way of writing the same thing.
   * "CDE[13]CDE"             plays three notes using the default Instrument #1, then again using Instrument #13.  
   * "(CEG)(DF#A)(EG#H)---"   plays three chords, the last of which is longer. 
   * "CDE&<<[28]efg&[110]  F" plays the three parts separated by &s simultaneously.   
   * "P:CDE"                  uses the MIDI percussion channel: each "note" represents a different percussion instrument. 
   * "C|D||||E"               means the same as ̀"cde": the |s don't do anything, but you can use them to mark bars or whatever.
   * 
   * Instruments: see the General MIDI Sound Set: http://www.midi.org/techspecs/gm1sound.php
   */
  def play(music: String): Unit = {
    Try(Music(music)) match {
      case Success(parsedMusic) => this.play(parsedMusic)
      case Failure(problem)     => println(s"Failed to play. Please check the string you used, which is repeated below:\n$music\nHere is the report from the parser: $problem")
    }
  }
  
  def play(music: Music) = {
    getLogger("java.util.prefs").setLevel(Level.SEVERE)
    val sequencer = MidiSystem.getSequencer  
    sequencer.open()                                         
    sequencer.setSequence(music.toMidi)                      
    sequencer.setTempoInBPM(music.tempo)                     
    sequencer.addMetaEventListener(new MetaEventListener { 
      def meta(message: MetaMessage) = if (message.getType == 0x2F) sequencer.close() 
    })
    Thread.sleep(MidiSystem.getSynthesizer.getLatency / 1000)
    sequencer.start() 
  }
  
  
  implicit class MusicLiteral(val noteData: StringContext) extends AnyVal {
    def music(embeddedBits: Any*) = Music(noteData.s(embeddedBits:_*))
  }
 
  
  // Supported formats: WAV, AIFF, AU, AIFC, SND
  def playRecording(id: String, repeats: Int = 0) = {
    val audioInput = AudioSystem.getAudioInputStream(tryForURL(id).get) 
    val soundToPlay = AudioSystem.getClip()
    soundToPlay.open(audioInput)
    soundToPlay.loop(repeats)
  }
  
  val KeepRepeating = Clip.LOOP_CONTINUOUSLY  

}

