package o1.TETRIS

import scala.math._
import scala.util.Random

class Tetris(board: Board) {
  val RLBlock = Array(Array(1,1,1),Array(1,0,0))
  val LLBlock = Array(Array(1,1,1),Array(0,0,1))
  val RSBlock = Array(Array(0,1,1),Array(1,1,0))
  val LSBlock = Array(Array(1,1,0),Array(0,1,1))
  val IBlock = Array(Array(1,1,1,1))
  val BBlock = Array(Array(1,1),Array(1,1))
  val WBlock = Array(Array(1,1,1), Array(0,1,0))
  val TESTBlock = Array(Array(1,1,1,1,1,1,1,1,1,1))
  val premade = Vector(RLBlock, LLBlock, WBlock, LSBlock, IBlock, BBlock, RSBlock)
  //Dropped block
  private var dropper: DropBlock = null
  //Score controlled by this Class and UnityBlock
  var bonusScore: Long = 0
  val enumerator = new Random()
 
 
  private def randomGrid: Array[Array[Int]] = {
    def index = enumerator.nextInt(13)
    if (index == 0) {
       val grid = UberRandomBlockifier.getAGrid(board.w)
      grid
      }
      else premade(index % 7)
  }
  private def makeABlock(grid: Array[Array[Int]]): movableBlock = {
    new movableBlock(grid)
  }
  
  def start = {
    initialize(makeABlock(randomGrid))
  }
  
  private def initialize(block: movableBlock): Boolean = {
    dropper = new DropBlock(block, 0, (board.w- block.width) /2, board)
    board.setBlock(block, 0, (board.w- block.width) / 2)
  }
  
  def rotate() = dropper.rotate()
  // Important one! Tries drop twice to counter some bugs. 
  def dropOne(): Boolean = {
    if (!dropper.drop) {
      if (!dropper.drop) {
        bonusScore += new UnityBlock(board).tetrisScore
        start
      }
      else {
        bonusScore += 1
        true
      }
    } 
    else {
      bonusScore += 1
      true
    }
  }
  def left() = dropper.moveLeft()
  def right() = dropper.moveRight()

  def wholeDrop(): Unit = {
    if (dropper.drop) {
      bonusScore += 2
      wholeDrop()     
    }
    else dropOne()
  }
  def reset() = {
    board.clearBoard()
    bonusScore = 0
    new UnityBlock(board)
    start
  }
}




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



class movableBlock(inputGrid: Array[Array[Int]]) extends Block(inputGrid) {
  
  private var rotator = 0
  private def makeRotate(grid: Array[Array[Int]]) = {
    val uusi = grid.transpose
   uusi.map(_.reverse)
  }
  
  private def massCenter(g: Array[Array[Int]]): (Int, Int) = {
    def getC(grid: Array[Array[Int]]): Int = {
    val Addition = grid.map(_.foldLeft(0)(_+_))
    val Mass = Addition.foldLeft(0)(_+_)
    val result = Addition.zipWithIndex.map(c => c._1 * c._2).foldLeft(0)(_+_).toDouble / Mass
     BigDecimal(result).setScale(0, BigDecimal.RoundingMode.HALF_DOWN).toInt
    }
    val y = getC(g)
    val x = getC(g.transpose)
    (y, x)
  }
  

  private val eastBlock = makeRotate(iGrid)
  private val southBlock = makeRotate(eastBlock)
  private val westBlock = makeRotate(southBlock)
  private val blockConfigs = Map(0 -> iGrid, 3  -> eastBlock, 2 -> southBlock, 1 -> westBlock)
  
  private val northMass = massCenter(iGrid)
  private val eastMass = massCenter(eastBlock)
  private val southMass = massCenter(southBlock)
  private val westMass = massCenter(westBlock)
  private val massConfigs = Map(0 -> northMass, 3 -> eastMass, 2 -> southMass, 1 -> westMass)
  
  def massFix = {
    val newMass = massConfigs(rotator % 4)
    val oldMass = massConfigs((rotator + 3) % 4)
    (-newMass._1 + oldMass._1, -newMass._2 + oldMass._2)
  }
  def rotate() = {
    rotator += 1
    iGrid = blockConfigs(rotator % 4)
  }
  def rotateBack() = {
    rotator -= 1
    iGrid = blockConfigs(rotator % 4)
  }
}



class Block(protected var iGrid: Array[Array[Int]]) {
  
  def currentGrid = iGrid
  def height = currentGrid.size
  def width = currentGrid(0).size  
}

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