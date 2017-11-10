
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1.sound

import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps


object MusicParser extends RegexParsers {
  
  def parse(musicString: String) = {
    val cleanedMusic = musicString.stripMargin(MarginStop).replaceAll(Ignored, "")
    this.parseAll(this.music, cleanedMusic) match {
      case Success  (parsedMusic, _) => parsedMusic
      case NoSuccess(message,     _) => throw new IllegalArgumentException(message)
    }
  }
  
  def music      = (voices | zeroVoices) ~ (tempo?)     ^^ { case voices~tempo           => Music(tempo, voices)                        }
  def tempo      = "/" ~> integer                                
  def moreVoices = ("&" ~> voice)*
  def zeroVoices = ""                                   ^^^ List[Voice]()
  def voices     = voice ~ moreVoices                   ^^ { case voice~voices           => voice :: voices                             }
  def voice      = ("[Pp]:".r?) ~ (songElem*)           ^^ { case percuss~elements       => Voice(elements, percuss.isDefined)          }
  def songElem   = (note|chord|pause|instrument|shift)
  def shift      = "<|>".r                              ^^ { case bracket                => OctaveShift(if (bracket == ">") 1 else -1)  }
  def pitch      = name ~ accidental ~ (octave?)        ^^ { case name~accidental~octave => Pitch(name, accidental, octave)             }
  def name       = keyName                              ^^ { case name                   => name.head                                   }
  def note       = pitch ~ length                       ^^ { case pitch~((len,stacc))    => Note(pitch, len, stacc)                     }
  def chord      = ("(" ~> (chordBit+) <~ ")") ~ length ^^ { case bits~((len,stacc))     => Chord(bits, len, stacc)                     }
  def chordBit   = pitch | shift
  def length     = "-*".r ~ ("."?)                      ^^ { case stay~staccato          => ((stay.length + 1) * 2, staccato.isDefined) }
  def instrument = "[" ~> integer <~ "]"                ^^ { case number                 => Instrument(number)                          }
  def octave     = "\\d".r                              ^^ { case digit                  => digit.toInt                                 }
  def integer    = "\\d+".r                             ^^ { case digits                 => digits.toInt                                }
  def accidental = flat | sharp | natural 
  def flat       = "b|♭|B".r                           ^^^ Flat    
  def sharp      = "#|♯".r                              ^^^ Sharp
  def natural    = ("♮"?)                               ^^^ Natural
  def pause      = " ".r                                ^^^ Pause 
  
  private val keyName = (Pitch.OctaveSemitones + Pitch.OctaveSemitones.toLowerCase).replaceAll(" ", "").mkString("|").r
  private val Ignored = raw"[_|\n\r]"
  private val MarginStop = '|'

  override val skipWhitespace = false

}

