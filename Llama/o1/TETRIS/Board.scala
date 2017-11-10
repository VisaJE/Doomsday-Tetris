package o1.TETRIS


class Board(val h: Int, val w: Int) {
 
  private var grid = Array.ofDim[Int](h, w)
  def gameGrid = grid
 
  def setBlock(block: Block, y: Int, x: Int):Boolean = {
    var newGrid: Array[Array[Int]] = copyGrid()
   
    if (x >= 0 && (block.width + x) <= w && y >= 0 && block.height + y <= h) {
      for (row <- 0 until block.height) {
        for (column <- 0 until block.width) {
          val newValue = block.currentGrid(row)(column) + this.grid(row + y)(column + x) 
          newGrid(row + y)(column + x) = newValue     
        }
      }
      if (newGrid.flatten.forall(x => x < 2 && x >= 0)) {
      this.grid = newGrid
      true
      }  else false
    } else false
  }
  
  def copyGrid() = {
    val grid2: Array[Array[Int]] = Array.ofDim[Int](h, w)
    for (a <- 0 until h) {
      for (b <- 0 until w) {
        grid2(a)(b) = this.grid(a)(b)
      }
    }
    grid2
  }
  
  def removeBlock(block: Block, y: Int, x: Int) = {
    for (row <- 0 until block.height) {
      if (h > row + y) {
        for (column <- 0 until block.width) {
          if (w > column + x) {
            grid(row + y)(column + x) -= block.currentGrid(row)(column)
          }
        }
      }
    }
  }
  def clearBoard() = {
    for (i <- 0 until h) {
      for (k <- 0 until w) {
        grid(i)(k) = 0
      }
    }
  }
}