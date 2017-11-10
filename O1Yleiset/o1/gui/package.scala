
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1


import o1.util.{localURL, Program}
import javax.swing.ImageIcon
import javax.imageio.ImageIO

package object gui {

  def makeImage(fileName: String) = localURL(fileName).map(ImageIO.read)

  
  trait TerminatesOnClose extends swing.Frame { 
    override def closeOperation() = {
      super.closeOperation()
      if (!Program.isRunningInScalaREPL) {
        System.exit(0)
      }
    }
  }
}
