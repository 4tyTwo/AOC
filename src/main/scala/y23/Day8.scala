package y23

import aoc.AoCProblem

case object Day8 extends AoCProblem[Long] {

  val day = 8

  private val input = scala.io.Source.fromFile("./inputs/2023/day8/input.txt").getLines().toSeq
  private val testInput1 = scala.io.Source.fromFile("./inputs/2023/day8/test_input1.txt").getLines().toSeq
  private val testInput2 = scala.io.Source.fromFile("./inputs/2023/day8/test_input2.txt").getLines().toSeq
  private val testInput3 = scala.io.Source.fromFile("./inputs/2023/day8/test_input3.txt").getLines().toSeq

  def parseLine(line: String): (String, (String, String)) = {
    val k = line.substring(0, 3)
    val v1 = line.substring(7, 10)
    val v2 = line.substring(12, 15)
    k -> (v1, v2)
  }

  def followDirections1(cond: String => Boolean, directions: String, i: Int, steps: Long, currPosition: String, lookup: Map[String, (String, String)]): Long = {

    def nextIdx: Int = {
      if (i < directions.length - 1) i + 1 else 0
    }

    if (cond(currPosition)) {
      steps
    } else {
      val next = directions(i) match {
        case 'L' => lookup(currPosition)._1
        case 'R' => lookup(currPosition)._2
      }
      followDirections1(cond, directions, nextIdx, steps + 1, next, lookup)
    }
  }

  def solve1: Long = {
    val directions = input.head
    val lookup = input.drop(2).map(parseLine).toMap
    followDirections1(_ == "ZZZ", directions, 0, 0, "AAA", lookup)
  }

  def solve2: Long = {
    val directions = input.head
    val lookup = input.drop(2).map(parseLine).toMap
    val startPositions = lookup.keys.filter(_.endsWith("A"))
    val ress = startPositions.map(followDirections1(_.endsWith("Z"), directions, 0, 0, _, lookup))
    println(ress.toSeq) // Calculate an LCM of those numbers. I just googled the calculator
    13385272668829L
  }

}
