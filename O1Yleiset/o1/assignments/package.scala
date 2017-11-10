
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1

import javax.swing._
import scala.util._
import scala.concurrent._
import scala.swing.Swing
import ExecutionContext.Implicits.global
import java.lang.reflect.InvocationTargetException
import scala.language.dynamics
import scala.swing.Component
import o1.gui.Dialog._
import scala.reflect.ClassTag
import o1.util._

package object assignments {

  // utils for reflection:
  trait ReflectionArgumentTag
  type Argument = Any @@ ReflectionArgumentTag
  def arg(any: Any) = javify(any).asInstanceOf[Argument] 
    
  private def javify(scalaVal: Any) = scalaVal match {
    case int: Int         => new java.lang.Integer(int)
    case double: Double   => new java.lang.Double(double)
    case char: Char       => new java.lang.Character(char)
    case boolean: Boolean => new java.lang.Boolean(boolean)
    case short: Short     => new java.lang.Short(short)
    case byte: Byte       => new java.lang.Byte(byte)
    case float: Float     => new java.lang.Float(float)
    case long: Long       => new java.lang.Long(long)
    case otherObject      => otherObject.asInstanceOf[AnyRef] 
  }  

  // Wrappers for the "dynamic" use of evolving and buggy student classes, implemented using Java reflection. 
  // Upgrade to Scala reflection when it loses its experimental status.

  class DynamicClass[Supertype <: AnyRef](private val className: String, private val constructorParameterTypes: Seq[Class[_]]) {

    private val actualClass = Try(Class.forName(className).asInstanceOf[Class[Supertype]]) recoverWith {
      case noSuchClass:  ClassNotFoundException => println("The class " + className + " was not available."); throw noSuchClass
      case invalidClass: ClassCastException     => invalidClass.printStackTrace()                           ; throw invalidClass
    }

    val isUsable = this.actualClass.isSuccess

    def instantiate(parameters: Argument*) = { 
      this.actualClass match {  
        case Failure(_) => 
          throw new InvalidSignature("The class " + this.className + " is not available and wasn't successfully instantiated.")
        case Success(actualClass) => 
          try {
            val constructor = actualClass.getConstructor(this.constructorParameterTypes:_*)
            constructor.newInstance(parameters:_*).asInstanceOf[Supertype]
          } catch {
            case problemWithinImplementation: InvocationTargetException =>
              println("The instantiation of an object of the class " + this.className + " failed to complete.")
              throw problemWithinImplementation
            case instantiationProblem: Exception => 
              throw new InvalidSignature("The class " + this.className + " wasn't successfully instantiated.")
          }
      }
    }
  }
  
  
  
  
  class DynamicObject[StaticType](private val wrapped: StaticType) extends Dynamic { 
    
    def applyDynamic[ResultType: ClassTag](methodName: String)(parameters: (Class[_], Argument)*) = {  
      val returnValue = try {
        val method = this.wrapped.getClass.getMethod(methodName, parameters.map( _._1 ): _*)
        method.invoke(this.wrapped, parameters.map( _._2 ): _*)
      } catch {
        case problemWithinImplementation: InvocationTargetException =>
          println("A call to the method " + methodName + " failed to complete.")
          throw problemWithinImplementation
        case otherProblem: Exception =>
          throw new InvalidSignature("The method or variable " + methodName + " was not successfully accessed.") 
      }
      
      val boxings = Map[Class[_], Class[_]](classOf[Boolean] -> classOf[java.lang.Boolean], classOf[Int] -> classOf[java.lang.Integer], classOf[Double] -> classOf[java.lang.Double],  classOf[Char] -> classOf[java.lang.Character],  classOf[Short] -> classOf[java.lang.Short],  classOf[Long] -> classOf[java.lang.Long],  classOf[Byte] -> classOf[java.lang.Byte],  classOf[Float] -> classOf[java.lang.Float])
      val expectedClassTag = implicitly[ClassTag[ResultType]]
      val expectedClass = expectedClassTag.runtimeClass
      if (expectedClassTag == ClassTag(classOf[Unit]) || expectedClass.isInstance(returnValue) || boxings.get(expectedClass).map( _.isInstance(returnValue) ).getOrElse(false)) 
        returnValue.asInstanceOf[ResultType]
      else 
        throw new InvalidSignature("The return value of " + methodName + " was not of the expected type.") 
    }
    
    def selectDynamic[ResultType](methodName: String)(implicit expectedClassTag: ClassTag[ResultType]) = {  
      this.applyDynamic[ResultType](methodName)()
    }
    
    def get[StaticType] = this.wrapped
    
  }


  class InvalidSignature(val message: String) extends Exception(message)
  
  
  trait RequestArguments[T <: AnyRef] {
    this: DynamicClass[T] =>
    
    protected val argumentRequesters: Seq[() => Option[Any]]

    def requestInstance() = {
      val responses = this.argumentRequesters.view.map( _() ).takeWhile( _.isDefined ).force.flatten
      if (responses.size < argumentRequesters.size) None else Some(this.instantiate(responses.map(arg): _*))
    }
    
  }

  
  def withStudentSolution(owner: Component)(task: =>Unit) = {
    try {
      task
    } catch {
      case studentCodeProducedException: InvocationTargetException => 
        throw studentCodeProducedException
      case solutionDoesntMeetSpecification: InvalidSignature =>  
        println(solutionDoesntMeetSpecification.message) 
        display("A part of the implementation was missing or invalid: " + solutionDoesntMeetSpecification.message, RelativeTo(owner))
    }
  }

  
  
}