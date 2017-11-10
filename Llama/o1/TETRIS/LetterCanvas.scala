package o1.TETRIS

import scala.swing._
import scala.swing.event._


class LetterCanvas(private val board: Board, size: Int) extends TextArea {
  this.editable = false
  focusable = false
  this.font = new Font("Arial", 0, size)
  this.background = new Color(220, 230, 240)
  this.border = Swing.LineBorder(new Color(10,10,0), size / 10)
  
  private def modify(grid: Array[Array[Int]], between: String, block: String): String = {
    val textArray: Array[Array[String]] = Array.ofDim[String](board.h, board.w)
    
      for (i <- 0 until textArray.size) {
        for (k <- 0 until textArray(0).size) {
          if  (grid(i)(k) % 2 == 0) {
            textArray(i)(k) = "  "
          }
          else textArray(i)(k) = block
        }
      }
    textArray.map(_.mkString(between)).mkString("\n")
  }
  def updateCanvas() = {
    this.text = modify(board.gameGrid, "|", "[]")
  }
  def pauseCanvas() = {
    this.text = modify(board.gameGrid, " ", "  ")
  }
  updateCanvas()
}