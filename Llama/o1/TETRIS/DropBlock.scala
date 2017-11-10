package o1.TETRIS

import scala.math._

class DropBlock(var block: movableBlock, private var yPos: Int, private var xPos: Int, board: Board) {
  //To prevent actions occurring at the same time
  private var processing = false
  
  def rotate() = {
    if (!processing) {
      processing = true
      var y = yPos
      var x = xPos
      board.removeBlock(block, y, x)
      block.rotate()
      val oldX = x
      y = max(y + block.massFix._1, 0)
      x += block.massFix._2
      if (!board.setBlock(block, y, x)) {
        var success = false
        val vary = math.ceil(block.width.toDouble / 2).toInt
        for (s <- 0 to vary) {
          if (!success) {
            x += 1
            success = board.setBlock(block, y, min(x, board.w - 1))
          }
        }
        if (!success) {
          x = oldX + block.massFix._2
          for (s  <- 0 to vary) {
            if (!success) {
              x -= 1
              success = board.setBlock(block,y, max(x, 0))
            }
          }
        }
        if (!success) {
        x = oldX
        y -= block.massFix._1
        block.rotateBack()
        board.setBlock(block, y, x)
        }
      } 
      yPos = y
      xPos = x
      processing = false
    }
  }
  def drop: Boolean = {
    if (!processing) {
      processing = true
    var y = yPos
    var x = xPos
    var result = true
    board.removeBlock(block, y, x)
    y += 1
    if (!board.setBlock(block, y, x)) {
      y -= 1
      result = !board.setBlock(block, y, x)
    }
    else result = true
    yPos = y
    xPos = x
    processing = false
    result
    }
    else false 
  }
  def moveLeft(): Boolean = {
    if (!processing) {
      var result = true
      var y = yPos
      var x = xPos
      board.removeBlock(block, y, x)
      x -= 1
      if (!board.setBlock(block, y, x)) {
        x += 1
        result = !board.setBlock(block, y, x)
      }
      xPos = x
      yPos = y
      processing = false
      result
      } else false
  }  
    def moveRight(): Boolean = {
      if (!processing) {
        var result = true
        var y = yPos
        var x = xPos
        board.removeBlock(block, y, x)
        x += 1
        if (!board.setBlock(block, y, x)) {
           x -= 1
           result = !board.setBlock(block, y, x)
        }
      xPos = x
      yPos = y
      processing = false
      result
      }  else false
    } 
  
}



class UnityBlock(board: Board) {
  var workingGrid = board.copyGrid
  workingGrid = workingGrid.filterNot(_.forall(_ == 1))
  val topGrid = Array.ofDim[Int](board.h - workingGrid.size, board.w)
  workingGrid = topGrid ++: workingGrid
  val unity = new Block(workingGrid)
  board.clearBoard()
  board.setBlock(unity, 0 ,0)
  
  def tetrisScore = topGrid.size * topGrid.size * 100
}
