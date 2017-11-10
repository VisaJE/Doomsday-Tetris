package o1.gui

import scala.swing._
import GridBagPanel.Anchor._

package object layout {

  type Anchor = GridBagPanel.Anchor.Value
  
  val OneSlot =   (1, 1)
  val TwoWide =   (2, 1)
  val ThreeWide = (3, 1)
  val TwoHigh =   (1, 2)
  val ThreeHigh = (1, 3)

  type Fill = (GridBagPanel.Fill.Value, Int, Int)
  def NoFill(xWeight: Int, yWeight: Int)   = (GridBagPanel.Fill.None, xWeight, yWeight)
  def FillVertical(weight: Int)            = (GridBagPanel.Fill.Vertical, 0, weight)
  def FillHorizontal(weight: Int)          = (GridBagPanel.Fill.Horizontal, weight, 0)
  def FillBoth(xWeight: Int, yWeight: Int) = (GridBagPanel.Fill.Both, xWeight, yWeight)
  val Slight = NoFill(0, 0)
  
  val NoBorder = (0, 0, 0, 0)
  
  trait EasyLayout extends LayoutContainer {
      
    def constraintsFor(component: Component, xy: (Int,Int), size: (Int, Int), anchor: Anchor, fill: Fill, border: (Int,Int,Int,Int)): Constraints

    def place(component: Component, xy: (Int,Int), size: (Int, Int), anchor: Anchor, fill: Fill, border: (Int,Int,Int,Int)) {
      this.layout += component -> constraintsFor(component, xy, size, anchor, fill, border)
    }
    
    def placeC(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, Center, fill, border)
    }
    def placeN(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, North, fill, border)
    }
    def placeE(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, East, fill, border)
    }
    def placeS(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, South, fill, border)
    }
    def placeW(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, West, fill, border)
    }
    def placeNW(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, NorthWest, fill, border)
    }
    def placeNE(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, NorthEast, fill, border)
    }
    def placeSW(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, SouthWest, fill, border)
    }
    def placeSE(component: Component, xy: (Int,Int), size: (Int, Int), fill: Fill, border: (Int,Int,Int,Int)) {
      this.place(component, xy, size, SouthEast, fill, border)
    }
  
  }

  trait EasyPanel extends GridBagPanel with EasyLayout {
    def constraintsFor(component: Component, xy: (Int,Int), size: (Int, Int), anchor: Anchor, fill: Fill, border: (Int,Int,Int,Int)) = 
      new Constraints(xy._1, xy._2, size._1, size._2, fill._2, fill._3, anchor.id, fill._1.id, new Insets(border._1, border._2, border._3, border._4), 0, 0)
  }
  
  
}