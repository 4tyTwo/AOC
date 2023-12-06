package y23;

import aoc.AoCProblem

import scala.util.matching.Regex

case object Day1 extends AoCProblem[Int] {

  val day = 1;

  private val input = scala.io.Source.fromFile("./inputs/2023/day1/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day1/test_input.txt").getLines().toSeq

  def solve1: Int = {
    input.map { s =>
      val digits =  s.filter(_.isDigit).map(_.toInt - 48)
      digits.head * 10 + digits.last
    }.sum
  }


  val digitNames = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  private def toInt(s: String) =
    digitNames.getOrElse(s, s.toInt)

  def solve2: Int = {

    val regex = "(one|two|three|four|five|six|seven|eight|nine|\\d){1}".r
    val reversedRegex = "(eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|\\d){1}".r
    input.map { s =>
      val first = regex.findFirstIn(s).get
      val last = reversedRegex.findFirstIn(s.reverse).map(_.reverse).get
      toInt(first) * 10 + toInt(last)
    }.sum
  }
}
