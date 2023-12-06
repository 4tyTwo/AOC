package y23

import aoc.AoCProblem

case object Day6 extends AoCProblem[Long] {

  val day = 6

  private val input = scala.io.Source.fromFile("./inputs/2023/day6/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day6/test_input.txt").getLines().toSeq


  def parsePart1Input(input: Seq[String]): Seq[(Long, Long)] = {
    val times = input.head.split(":").last.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toLong)
    val distances = input.last.split(":").last.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toLong)

    times.zip(distances).toSeq
  }

  def parsePart2Input(input: Seq[String]): (Long, Long) = {
    val time = input.head.split(":").last.filter(_ != ' ').toLong
    val distance = input.last.split(":").last.filter(_ != ' ').toLong
    (time, distance)
  }

  private def solveQuadraticEquationForLongs(a: Long, b: Long, c: Long): (Long, Long) = {
    val D = math.pow(b, 2) - (4 * a * c)

    val t1 = (-b + math.sqrt(D)) / (2 * a)
    val t2 = (-b - math.sqrt(D)) / (2 * a)

    val min0 = math.min(t1, t2)
    val max0 = math.max(t1, t2)

    val min = if (math.ceil(min0) == min0) (min0 + 1).toLong else math.ceil(min0).toLong
    val max = if (math.floor(max0) == max0) (max0 - 1).toLong else math.floor(max0).toLong
    (min, max)
  }

  private def solveRace(time: Long, distance: Long): (Long, Long) = {
    solveQuadraticEquationForLongs(-1, time, -distance)
  }

  def solve1: Long = {
    parsePart1Input(input).map { case (t, d) =>
      val (min, max) = solveRace(t, d)
      max - min + 1
    }.product
  }

  def solve2: Long = {
    val (time, distance) = parsePart2Input(input)
    val (min, max) = solveRace(time, distance)
    max - min + 1
  }
}
