package Day02

object Day02 {
  def main(args: Array[String]) {
    import scala.io.Source

    val boxIds = Source.fromFile(args(0)).getLines().toList

    println(checkSum(boxIds))

    val (combination, position) = compareStringToRestOfList(boxIds)
    if (position == -1) println("No combination found")
    else println(s"Common letters are: $combination and strings differ in position: $position")
  }

  private def checkSum(boxIds: List[String]): Int =
    boxIds.foldLeft(0)(_ + isTuple(_, 2)) * boxIds.foldLeft(0)(_ + isTuple(_, 3))

  private def isTuple(boxId: String, repeated: Int): Int =
    boxId match {
      case "" => 0
      case string =>
        val count: Int = boxId.count(_ == boxId(0))
        if (count == repeated) 1
        else isTuple(boxId.filter(_ != boxId(0)), repeated)
    }

  private def compareStringToRestOfList(boxIds: List[String]): (String, Int) =
    boxIds match {
      case Nil => ("", -1)
      case head :: tail =>
        val (string, where) = getCombination(tail, head)
        if (where > -1) (string, where)
        else compareStringToRestOfList(tail)
    }

  private def getCombination(boxIds: List[String], boxId: String): (String, Int) =
    boxIds match {
      case Nil => ("", -1)
      case head :: tail =>
        val (string, where) = compare(boxId, head, 1, "", -1, 0)
        if (where == -1) getCombination(tail, boxId)
        else (string, where)
    }

  private def compare(string1: String,
                      string2: String,
                      differences: Int,
                      result: String,
                      where: Int,
                      index: Int): (String, Int) =
    (string1, string2) match {
      case ("", "") =>
        if (differences == 0) (result, where)
        else ("", -1)
      case ("", _) => ("", -1)
      case (_, "") => ("", -1)
      case (str1, str2) =>
        if (str1(0) == str2(0))
          compare(str1.substring(1),
                  str2.substring(1),
                  differences,
                  result + str1(0),
                  where,
                  index + 1)
        else if (differences == 0) ("", -1)
        else
          compare(str1.substring(1),
                  str2.substring(1),
                  differences - 1,
                  result,
                  index,
                  index + 1)
    }
}
