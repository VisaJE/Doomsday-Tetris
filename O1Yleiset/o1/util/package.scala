
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1

import scala.language.reflectiveCalls
import scala.util._
import scala.io.Source
import java.net.URL
import java.io.FileNotFoundException

package object util {

  
  def useAndClose[Resource <: Closeable, Result](resource: Resource)(operation: Resource => Result) = {
    try {
      operation(resource)
    } finally {
      resource.close()
    }
  }

  type Closeable = { def close(): Unit }

  
  implicit class ConvenientCollection[Element](val underlying: Traversable[Element]) {
    def mapify[Key, Value](formKey: Element => Key)(formValue: Element => Value): Map[Key, Value] =
      this.underlying.map( elem => formKey(elem) -> formValue(elem) )(collection.breakOut)
  }
  
  implicit class ConvenientInt(val underlying: Int) {
    def atLeast(minimum: Int) = this.underlying.max(minimum)
    def atMost(maximum: Int) = this.underlying.min(maximum)
  }
  
  

  def editDistance(text1: String, text2: String, threshold: Int): Int =
    if (text1.isEmpty)
      if (text2.length <= threshold) text2.length else Int.MaxValue
    else if (text2.isEmpty)
      if (text1.length <= threshold) text1.length else Int.MaxValue
    else if (text1.head == text2.head)
      editDistance(text1.tail, text2.tail, threshold)
    else if (threshold == 0) 
      Int.MaxValue 
    else { 
      val deletion     = editDistance(text1.tail, text2     , threshold - 1)
      val insertion    = editDistance(text1,      text2.tail, threshold - 1)
      val substitution = editDistance(text1.tail, text2.tail, threshold - 1)
      val shortest = Seq(deletion, insertion, substitution).min
      if (shortest == Int.MaxValue) Int.MaxValue else shortest + 1
    }

  
  // these two lines are based on http://etorreborre.blogspot.fi/2011/11/practical-uses-for-unboxed-tagged-types.html
  type Tagged[TagType] = { type Tag = TagType }
  type @@[BaseType, TagType] = BaseType with Tagged[TagType]


  
  private def source(url: URL): Source = Source.fromURL(url, "UTF-8") 
  
  def localURL(id: String)    = Option(this.getClass.getResource("/" + id))
  def localSource(id: String) = localURL(id).map(source)
  
  private def tryForResource[Result](id: String, transform: URL => Result) = {
    def withinWorkingDir(filename: String) = Try { localURL(filename).map(transform).getOrElse(throw new FileNotFoundException) }
    def url(url: String, prefix: String)   = Try { transform(new URL(prefix + url)) }
    withinWorkingDir(id) orElse url(id, "") orElse url(id, "http://") orElse url(id, "https://")
  }
  def tryForURL(id: String)    = tryForResource(id, identity) 
  def tryForSource(id: String) = tryForResource(id, source)
  
  

  
  implicit class RegexContext(interpolated: StringContext) {
    import scala.util.matching.Regex
    def r = new Regex(interpolated.parts.mkString, interpolated.parts.tail.map( _ => "unnamedGroup" ): _*)
  }

  
  object Program { // not entirely reliable, but good enough for some purposes
    lazy val isRunningInScalaREPL = Try(Class.forName("scala.tools.nsc.interpreter.IMain")).isSuccess 
  }
  
}
