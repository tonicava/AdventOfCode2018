package Day03

import math.max
import scala.util.matching.Regex

object Day03 {
  def main(args: Array[String]) {
    import scala.io.Source

    val numberRegex = """\d+""".r
    val lines = Source.fromFile(args(1)).getLines().toList.map(numberRegex.findAllMatchIn(_).toArray)
    val (maxX, maxY) = getMaxDimensions(lines, 0, 0)
    val fabric = Array.ofDim[Int](maxX, maxY)
    val suit = getSuit(fabric, lines)

    println(getOverlaps(suit, maxX, maxY))

    val patchNumber = getNotOverlapping(suit, lines)
    if(patchNumber == -1)
      println("no patch does not overlap")
    else
      println(patchNumber)
  }

  private def getMaxDimensions(lines: List[Array[Regex.Match]], maxX: Int, maxY: Int): (Int, Int) =
    lines match {
      case Nil => (maxX, maxY)
      case head::tail =>
        getMaxDimensions(
          tail,
          max(maxX, head(1).toString.toInt+head(3).toString.toInt),
          max(maxX, head(2).toString.toInt+head(4).toString.toInt)
        )
    }

  private def getSuit(fabric: Array[Array[Int]], lines: List[Array[Regex.Match]]): Array[Array[Int]] =
    lines match {
      case Nil => fabric
      case head::tail =>
        val xFrom = head(1).toString.toInt
        val xTo = xFrom + head(3).toString.toInt
        val yFrom = head(2).toString.toInt
        val yTo = yFrom + head(4).toString.toInt
        for {
          x <- xFrom until xTo
          y <- yFrom until yTo
        } fabric(x)(y) match {
          case 0 => fabric(x)(y) = 1
          case _ => fabric(x)(y) = 2
        }
        getSuit(fabric, tail)
    }

  private def getOverlaps(fabric: Array[Array[Int]], rows: Int, columns: Int): Int = {
    var overlaps = 0
    for {
      x <- 0 until rows
      y <- 0 until columns
    } if (fabric(x)(y) == 2) overlaps += 1

    overlaps
  }

  private def getNotOverlapping(fabric: Array[Array[Int]], lines: List[Array[Regex.Match]]): Int =
    lines match {
      case Nil => -1
      case head::tail =>
        val xFrom: Int = head(1).toString.toInt
        val xTo: Int = xFrom + head(3).toString.toInt
        val yFrom: Int = head(2).toString.toInt
        val yTo: Int = yFrom + head(4).toString.toInt
        if(notOverlapping(xFrom, xTo, yFrom, yFrom, yTo, fabric))
          head(0).toString.toInt
        else
            getNotOverlapping(fabric, tail)
    }

  private def notOverlapping(x: Int, xTo: Int, y: Int, yFrom: Int, yTo: Int, fabric: Array[Array[Int]]): Boolean =
    if(x == xTo - 1 && y == yTo) true
    else if(x < xTo - 1 && y == yTo) notOverlapping(x+1, xTo, yFrom, yFrom, yTo, fabric)
    else
        if(fabric(x)(y) != 1) false
        else notOverlapping(x, xTo, y+1, yFrom, yTo, fabric)
}