
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package o1.gui

import o1.util.localURL
import javax.swing.Icon
import javax.swing.ImageIcon


class ScaledPic(val filename: String, val width: Int, val height: Int) extends Icon {

  private val imageIcon = this.makeIcon()
  
  private def scale(icon: ImageIcon) = 
    new ImageIcon(icon.getImage.getScaledInstance(this.width, this.height, java.awt.Image.SCALE_AREA_AVERAGING))
    
  private def makeIcon() = {
    val original = localURL(this.filename).map( new ImageIcon(_) )
    original.map(this.scale)
  }
  
  def paintIcon(target: java.awt.Component, graphics: java.awt.Graphics, height: Int, width: Int) = {
    for (icon <- this.imageIcon) {
      icon.paintIcon(target, graphics, height, width)
    }
  }

  def getIconWidth = this.imageIcon.map( _. getIconWidth ).getOrElse(0)

  def getIconHeight = this.imageIcon.map( _.getIconHeight ).getOrElse(0)
  
  override def toString = this.filename + " scaled to " + this.width + " by " + this.height + " pixels" 
  
}

