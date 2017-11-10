package o1.TETRIS


class Block(protected var iGrid: Array[Array[Int]]) {
  
  def currentGrid = iGrid
  def height = currentGrid.size
  def width = currentGrid(0).size  
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