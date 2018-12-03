package Day01

object Day01 {
  def main(args: Array[String]) {
    import scala.io.Source

    val variations = Source.fromFile(args(0)).getLines().toList.map(_.toInt)

    println(finalFrequency(variations))
    println(firstRepeatedFrequency(variations.toArray, List(0), 0, 0))
  }

  private def finalFrequency(variations: List[Int]): Int = variations.sum

  private def firstRepeatedFrequency(variations: Array[Int],
                                     frequencies: List[Int],
                                     index: Int,
                                     sum: Int): Int = {
    val newSum: Int = sum + variations(index)
    if (frequencies.contains(newSum)) newSum
    else
      firstRepeatedFrequency(
        variations,
        newSum :: frequencies,
        (index + 1) % variations.length,
        newSum
      )
  }
}
