
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1.sound

import javax.sound.midi._

object Music {
  val PPQ               = 48
  val DefaultTempo      = 120
  val DefaultOctave     = 5
  val MaxVoices         = 15
  val LengthPerBeat     = 4
  val PercussionChannel = 9
  val HighVolume        = 127
  val MediumVolume      = 80
  def trackPosition(position: Int) = PPQ * position / LengthPerBeat
  def apply(musicString: String): Music = MusicParser.parse(musicString)
}


case class Music(val tempoSetting: Option[Int], val voices: Seq[Voice]) {
  import Music._
    
  if (tempoSetting.exists( _ <= 0 ))                       throw new IllegalArgumentException(s"Tempo needs to be a positive integer.")
  if (voices.filterNot( _.isPercussion ).size > MaxVoices) throw new IllegalArgumentException(s"A maximum of $MaxVoices tracks in parallel plus percussion track allowed.")
  if (voices.filter( _.isPercussion ).size > 1)            throw new IllegalArgumentException("No more than one percussion track allowed.")
    
  lazy val tempo = tempoSetting.getOrElse(DefaultTempo)
   
  def play() = {
    o1.sound.play(this)
  }
  
  def toMidi = {
    val seq = new Sequence(Sequence.PPQ, PPQ)
    val (percussion, regular) = this.voices.partition( _.isPercussion )
    regular.filter( _.nonEmpty ).foreach( _.addToSequence(seq) )
    percussion.foreach( _.addToSequence(seq) )
    seq
  }

}

case class Voice(val notes: Seq[MusicElem], isPercussion: Boolean) {
  import Music._
  import javax.sound.midi.ShortMessage._

  def nonEmpty = this.notes.nonEmpty
  
  lazy val length = this.notes.map( _.length ).sum
  
  def addToSequence(seq: Sequence) = {
    val nextFree    = seq.getTracks.size
    val trackNumber = if (this.isPercussion) PercussionChannel else if (nextFree < PercussionChannel) nextFree else nextFree + 1 
    val track       = seq.createTrack()
    track.add(new MidiEvent(new ShortMessage(STOP), trackPosition(this.length + PPQ / 4)))
    var position      = 0
    var currentOctave = DefaultOctave 

    def addEvent(onOrOff: Int, note: Note, position: Int) = {
      track.add(new MidiEvent(new ShortMessage(onOrOff, trackNumber, note.pitch.midiID(currentOctave), note.volume), trackPosition(position)))
    }
    
    def processElement(element: MusicElem): Unit = {
      element match {
        case note: Note         => addEvent(NOTE_ON, note, position); addEvent(NOTE_OFF, note, position + note.audibleLength) 
        case chord: Chord       => chord.notes.foreach(processElement)
        case Instrument(number) => track.add(new MidiEvent(new ShortMessage(PROGRAM_CHANGE, trackNumber, number - 1, 0), trackPosition(position))) 
        case OctaveShift(shift) => currentOctave += shift
        case Pause              => // nothing
      }
    }
  
    for (element <- this.notes) {
      processElement(element)
      position += element.length
    }
  }
}


object Pitch {
  val OctaveSemitones = "C D EF G A H"
  val MinID           = 0
  val MaxID           = 127
}
case class Pitch(val name: Char, val accidental: Accidental, val octave: Option[Int]) extends OccursInChord {
  import Pitch._
  val numberWithinOctave = OctaveSemitones.indexOf(this.name.toUpper)

  def midiID(defaultOctave: Int) = {
    val octave = this.octave.getOrElse(defaultOctave)
    val octaveStartID = OctaveSemitones.size * octave
    val globalNumber = octaveStartID + this.numberWithinOctave + this.accidental.shift
    globalNumber.max(MinID).min(MaxID)
  }

  def whenInChord(chordLength: Int, chordIsStaccato: Boolean) = Note(this, chordLength, chordIsStaccato)
  
}

trait Accidental { def shift: Int }
case object Flat    extends Accidental { val shift = -1 }
case object Sharp   extends Accidental { val shift = 1  }
case object Natural extends Accidental { val shift = 0  }

trait MusicElem {
  def length: Int
}

case class Note(val pitch: Pitch, val length: Int, isStaccato: Boolean) extends MusicElem {
  def audibleLength = if (this.isStaccato) this.length / 2 else this.length 
  def volume        = if (this.pitch.name.isUpper) Music.HighVolume else Music.MediumVolume
}

trait OccursInChord {
  def whenInChord(chordLength: Int, chordIsStaccato: Boolean): MusicElem
}

case class Chord(val notes: Seq[MusicElem]) extends MusicElem {
  def length = this.notes.map( _.length ).max
}
object Chord {
  def apply(chordBits: Seq[OccursInChord], length: Int, isStaccato: Boolean): Chord = 
    Chord(chordBits.map( _.whenInChord(length, isStaccato) ))
}

case class Instrument(val number: Int) extends MusicElem {
  if (number < 1 || number > 128) throw new IllegalArgumentException(s"The instrument number $number is not between 1 and 128 as expected.")
  def length = 0
}

case object Pause extends MusicElem {
  val length = 2
}
  
case class OctaveShift(val shift: Int) extends MusicElem with OccursInChord {
  def length = 0
  def whenInChord(chordLength: Int, chordIsStaccato: Boolean) = this
}

object Instrument {
  // From: http://www.midi.org/techspecs/gm1sound.php
  
  object Piano {
    val AcousticGrandPiano  = 1
    val BrightAcousticPiano = 2
    val ElectricGrandPiano  = 3
    val HonkyTonkPiano      = 4
    val ElectricPiano1      = 5
    val ElectricPiano2      = 6
    val Harpsichord         = 7
    val Clavi               = 8
  }
  object ChromaticPercussion {
    val Celesta      = 9
    val Glockenspiel = 10
    val MusicBox     = 11
    val Vibraphone   = 12
    val Marimba      = 13
    val Xylophone    = 14
    val TubularBells = 15
    val Dulcimer     = 16
  }
  object Organ {
    val DrawbarOrgan    = 17
    val PercussiveOrgan = 18
    val RockOrgan       = 19
    val ChurchOrgan     = 20
    val ReedOrgan       = 21
    val Accordion       = 22
    val Harmonica       = 23
    val TangoAccordion  = 24
  }
  object Guitar {
    val AcousticGuitarNylon = 25
    val AcousticGuitarSteel = 26
    val ElectricGuitarJazz  = 27
    val ElectricGuitarClean = 28
    val ElectricGuitarMuted = 29
    val OverdrivenGuitar    = 30
    val DistortionGuitar    = 31
    val Guitarharmonics     = 32
  }
  object Bass {
    val AcousticBass       = 33
    val ElectricBassFinger = 34
    val ElectricBassPick   = 35
    val FretlessBass       = 36
    val SlapBass1          = 37
    val SlapBass2          = 38
    val SynthBass1         = 39
    val SynthBass2         = 40
  }
  object Strings {
    val Violin           = 41
    val Viola            = 42
    val Cello            = 43
    val Contrabass       = 44
    val TremoloStrings   = 45
    val PizzicatoStrings = 46
    val OrchestralHarp   = 47
    val Timpani          = 48
  }
  object Ensemble {
    val StringEnsemble1 = 49
    val StringEnsemble2 = 50
    val SynthStrings1   = 51
    val SynthStrings2   = 52
    val ChoirAahs       = 53
    val VoiceOohs       = 54
    val SynthVoice      = 55
    val OrchestraHit    = 56
  }
  object Brass {
    val Trumpet      = 57
    val Trombone     = 58
    val Tuba         = 59
    val MutedTrumpet = 60
    val FrenchHorn   = 61
    val BrassSection = 62
    val SynthBrass1  = 63
    val SynthBrass2  = 64
  }
  object Reed {
    val SopranoSax  = 65
    val AltoSax     = 66
    val TenorSax    = 67
    val BaritoneSax = 68
    val Oboe        = 69
    val EnglishHorn = 70
    val Bassoon     = 71
    val Clarinet    = 72
  }
  object Pipe {
    val Piccolo     = 73
    val Flute       = 74
    val Recorder    = 75
    val PanFlute    = 76
    val BlownBottle = 77
    val Shakuhachi  = 78
    val Whistle     = 79
    val Ocarina     = 80
  }
  object SynthLead {
    val Square       = 81
    val Sawtooth     = 82
    val Calliope     = 83
    val Chiff        = 84
    val Charang      = 85
    val Voice        = 86
    val Fifths       = 87
    val BassPlusLead = 88
  }
  object SynthPad {
    val NewAge    = 89
    val Warm      = 90
    val Polysynth = 91
    val Choir     = 92
    val Bowed     = 93
    val Metallic  = 94
    val Halo      = 95
    val Sweep     = 96
  }
  object SynthEffects {
    val Rain       = 97
    val Soundtrack = 98
    val Crystal    = 99
    val Atmosphere = 100
    val Brightness = 101
    val Goblins    = 102
    val Echoes     = 103
    val SciFi      = 104
  }
  object Ethnic {
    val Sitar    = 105
    val Banjo    = 106
    val Shamisen = 107
    val Koto     = 108
    val Kalimba  = 109
    val Bagpipe  = 110
    val Fiddle   = 111
    val Shanai   = 112
  }
  object Percussion {
    val TinkleBell    = 113
    val Agogo         = 114
    val SteelDrums    = 115
    val Woodblock     = 116
    val TaikoDrum     = 117
    val MelodicTom    = 118
    val SynthDrum     = 119
    val ReverseCymbal = 120
  }
  object SoundEffects {
    val GuitarFretNoise = 121
    val BreathNoise     = 122
    val Seashore        = 123
    val BirdTweet       = 124
    val TelephoneRing   = 125
    val Helicopter      = 126
    val Applause        = 127
    val Gunshot         = 128
  }
  
  
  object PercussionChannel {
    val AcousticBassDrum = "H2"
    val BassDrum         = "C3"
    val SideStick        = "C#3"
    val AcousticSnare    = "D3"
    val HandClap         = "D#3"
    val ElectricSnare    = "E3"
    val LowFloorTom      = "F3"
    val ClosedHiHat      = "F#3"
    val HighFloorTom     = "G3"
    val PedalHiHat       = "G#3"
    val LowTom           = "A3"
    val OpenHiHat        = "A#3"
    val LowMidTom        = "H3"
    val HiMidTom         = "C4"
    val CrashCymbal1     = "C#4"
    val HighTom          = "D4"
    val RideCymbal1      = "D#4"
    val ChineseCymbal    = "E4"
    val RideBell         = "F4"
    val Tambourine       = "F#4"
    val SplashCymbal     = "G4"
    val Cowbell          = "G#4"
    val CrashCymbal2     = "A4"
    val Vibraslap        = "A#4"
    val RideCymbal2      = "H4"
    val HiBongo          = "C5"
    val LowBongo         = "C#5"
    val MuteHiConga      = "D5"
    val OpenHiConga      = "D#5"
    val LowConga         = "E5"
    val HighTimbale      = "F5"
    val LowTimbale       = "F#5"
    val HighAgogo        = "G5"
    val LowAgogo         = "G#5"
    val Cabasa           = "A5"
    val Maracas          = "A#5"
    val ShortWhistle     = "H5"
    val LongWhistle      = "C6"
    val ShortGuiro       = "C#6"
    val LongGuiro        = "D6"
    val Claves           = "D#6"
    val HiWoodBlock      = "E6"
    val LowWoodBlock     = "F6"
    val MuteCuica        = "F#6"
    val OpenCuica        = "G6"
    val MuteTriangle     = "G#6"
    val OpenTriangle     = "A6"
  }
  
}
