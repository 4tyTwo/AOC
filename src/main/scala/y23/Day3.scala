package y23

import aoc.AoCProblem

case object Day3 extends AoCProblem[Int] {
  val day = 3

  private val input = scala.io.Source.fromFile("./inputs/2023/day3/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day3/test_input.txt").getLines().toSeq


  private def surroundedBySymbol(currLine: Int, start: Int, end: Int, lines: Seq[String]): Boolean = {
    def isSymbol(c: Char): Boolean = c match {
      case _ if c.isDigit => false
      case '.' => false
      case _ => true
    }

    val hasSymbolAbove = if (currLine > 0) {
      (math.max(0, start -1 ) to math.min(lines.head.length - 1, end))
        .foldLeft(false) { case (acc, i) =>
          acc || isSymbol(lines(currLine - 1)(i))
      }
    } else false

    val hasSymbolLeftRight = {
      val symbolLeft = if (start > 0) { isSymbol(lines(currLine)(start - 1)) } else false
      val symbolRight = if (end < lines.head.length) { isSymbol(lines(currLine)(end)) } else false
      symbolLeft || symbolRight
    }

    val hasSymbolBelow = if (currLine < lines.length - 1) {
      (math.max(0, start - 1) to math.min(lines.head.length - 1, end))
        .foldLeft(false) { case (acc, i) =>
          acc || isSymbol(lines(currLine + 1)(i))
        }
    } else false

    hasSymbolAbove || hasSymbolLeftRight || hasSymbolBelow
  }

  def solve1 = {
    input.indices.map { i =>
      "\\d+".r.findAllMatchIn(input(i)).map { m =>
        val curr = input(i).substring(m.start, m.end).toInt
        if (surroundedBySymbol(i, m.start, m.end, input)) {curr} else 0
      }.sum
    }.sum
  }


  case class Number(value: Int, start: Int, end: Int)

  def solve2 = {
    val numbersMap = input.zipWithIndex.map { case (line, idx) =>
      idx -> "\\d+".r.findAllMatchIn(line).map(m => Number(line.substring(m.start, m.end).toInt, m.start, m.end - 1)).toSeq
    }.toMap

    input.zipWithIndex.map { case (line, i) =>
      line.zipWithIndex.map { case (char, j) =>
        if (char == '*') {
          val numbersAbove = numbersMap.getOrElse(i - 1, Seq.empty).filter { case Number(_, start, end) =>
            (start to end).contains(j)|| start == j + 1 || end == j - 1
          }
          val numbersBelow = numbersMap.getOrElse(i + 1, Seq.empty).filter { case Number(_, start, end) =>
            (start to end).contains(j)|| start == j + 1 || end == j - 1
          }

          val left = numbersMap.getOrElse(i, Seq.empty).filter { case Number(_, _, end) =>
            end == j - 1
          }

          val right = numbersMap.getOrElse(i, Seq.empty).filter { case Number(_, start, _) =>
            start == j + 1
          }

          val surroundingNumbers = numbersAbove ++ numbersBelow ++ left ++ right
          if (surroundingNumbers.length == 2) {
            surroundingNumbers.map(_.value).product
          } else 0
        }
        else 0
      }.sum
    }.sum
  }
}
