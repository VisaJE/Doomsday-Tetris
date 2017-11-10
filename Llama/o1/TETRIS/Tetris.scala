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













