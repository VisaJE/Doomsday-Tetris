package o1.TETRIS

  import scala.math._
  import scala.util.Random
  
object UberRandomBlockifier {
  private val gen = new Random()
  def getSome(a: Int): Int = {
    try {
    gen.nextInt(a)
    } catch {
      case uno: Exception => 0
      }
  }
  private val defMaxSize = 10
  //Unevenly distributed random number generator. Does not generate 0.
   
    private def randomount(max: Int) = {
    val maxFix = scala.math.max(max, 1)
    val base: Double = pow(1.2, - (max / 190.0) + 18.0 / 19)
    val numbers = (1 to maxFix).toVector.zipWithIndex.reverse
    val weights = numbers.map(x => (x._1, pow(base,x._2).toInt))
    val wholeLoad = weights.foldLeft(0)(_ + _._2)
    val choice = getSome(wholeLoad)
    val take = (log(choice * (base - 1) + 1)  / log(base)).toInt
    weights(take)._1
  }
  
  def getAGrid(maxSize:Int) = {
    var usableBlocks: Int = 0
    var blockMap: Map[Int, (Int, Int)] = Map()
    val maxWidth = randomount(maxSize)
    val maxHeight = randomount(maxWidth)
    val maxBlocks = maxWidth * maxHeight
    usableBlocks = max(randomount(maxWidth * maxHeight), 1)
    blockMap += 0 -> randomCoords(maxHeight, maxWidth)
    usableBlocks -= 1
    var index = 1
    while(usableBlocks > 0) {
      val all = allEmptyNeighbors(blockMap, maxHeight, maxWidth)
      blockMap += index -> all(getSome(all.size - 1))
      index += 1
      usableBlocks -= 1
    }
    trim(blockMap.values.toVector)
  }
  def getAGrid(): Array[Array[Int]] = getAGrid(defMaxSize)
  
  private def trim(a: Vector[(Int, Int)]): Array[Array[Int]] = {
    val minY: Int = a.map(_._1).reduceLeft(min(_, _))
    val minX: Int = a.map(_._2).reduceLeft(min(_, _))
    val b1 = a.map((c: (Int, Int)) => (c._1 - minY, c._2))
    val b = b1.map((c: (Int, Int)) => (c._1, c._2 - minX))
    val h = b.map(_._1).reduceLeft(max(_, _)) + 1
    val w = b.map(_._2).reduceLeft(max(_, _)) + 1
    val output = Array.ofDim[Int](h, w)
    b.foreach(x => output(x._1)(x._2) = 1)
    if (h > w) output.transpose
    else output
  }
  private def randomCoords(mh: Int, mw: Int) = {
    (getSome(mh), getSome(mw))
  }
  private def emptyNeighbors(input: Map[Int, (Int, Int)], number: Int, mh: Int, mw: Int): Vector[(Int, Int)] = {
    val targetX: Int = input(number)._2
    val targetY: Int = input(number)._1
    val neighborList = input.filter(a => a._2._1 == targetY + 1 || a._2._1 == targetY - 1 && a._2._2 == targetX ^ a._2._2 == targetX + 1 || a._2._2 == targetX - 1 && a._2._1 == targetY).values.toVector
    var possibilities = Vector((targetY + 1, targetX), (targetY - 1, targetX), (targetY, targetX - 1), (targetY, targetX + 1))
    possibilities = possibilities.filterNot(a => neighborList.exists(_ == a))
    possibilities.filterNot(a => a._1 < 0 || a._2 < 0 || a._1 >= mh || a._2 >= mw)
  }
  //Same coords might appear twice
  private def allEmptyNeighbors(input: Map[Int, (Int, Int)], mh: Int, mw: Int): Vector[(Int, Int)] = {
    var result: Vector[(Int, Int)] = Vector()
    for (i <- input.keys) {
      result = result ++ emptyNeighbors(input, i, mh, mw)
    }
    result
  }
  
}