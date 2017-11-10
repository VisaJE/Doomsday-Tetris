
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1.gui

import scala.util.Try
import scala.swing._
import scala.swing.Dialog._


object Dialog {

  def display(message: String, position: Position) = {
    showMessage(position.javaParent, message, "", Message.Plain, Swing.EmptyIcon)
  }
  
  def requestInput[ResultType](prompt: String, convert: String => ResultType, isOk: ResultType => Boolean, errorMessage: String, position: Position) = {
    val isReallyOk = (input: String) => Try(isOk(convert(input))).getOrElse(false)
    val parent = position.javaParent
    var inputLine = showInput(parent, prompt, prompt, Message.Question, Swing.EmptyIcon, Nil, "")
    while (inputLine.exists( !isReallyOk(_) )) {
      inputLine = showInput(parent, errorMessage + "\n" + prompt, prompt, Message.Error, Swing.EmptyIcon, Nil, "")
    }
    inputLine.map( convert(_) )
  }
  
  def requestString(prompt: String, isOk: String => Boolean, errorMessage: String, position: Position) = {
    requestInput(prompt, _.trim, isOk, errorMessage, position)
  }
  
  
  def requestAnyLine(prompt: String, position: Position) = {
    requestString(prompt, AnythingGoes, "", position)
  }
  
  def requestNonEmptyLine(prompt: String, errorMessage: String, position: Position) = {
    requestString(prompt, !_.isEmpty, errorMessage, position)
  }

  def requestInt(prompt: String, isOk: Int => Boolean, errorMessage: String, position: Position) = {
    requestInput(prompt, _.toInt, isOk, errorMessage, position)
  }
  
  def requestAnyInt(prompt: String, errorMessage: String, position: Position) = {
    requestInt(prompt, AnythingGoes, errorMessage, position)
  }
  
  private val AnythingGoes = (_: Any) => true 
  
  def requestDouble(prompt: String, isOk: Double => Boolean, errorMessage: String, position: Position) = {
    requestInput(prompt, _.toDouble, isOk, errorMessage, position)
  }
  
  def requestAnyDouble(prompt: String, position: Position) = {
    requestDouble(prompt, AnythingGoes, "", position)
  }
  
  def requestChoice[Choice](prompt: String, options: Seq[Choice], position: Position) = {
    showInput(position.javaParent, prompt, prompt, Message.Question, Swing.EmptyIcon, options, options.head) 
  } 
      
  sealed trait Position {
    def parent: Option[Component]
    def javaParent = this.parent.orNull
  }
  case class RelativeTo(val locator: Component) extends Position {
    def parent = Some(locator)
  }
  case object Centered extends Position {
    def parent = None
  } 
  object RelativeTo {
    def apply(frame: Frame) = frame.contents.headOption.map(new RelativeTo(_)).getOrElse(Centered)
  }
  
}
 
