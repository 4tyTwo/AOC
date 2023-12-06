package y23
import aoc.AoCProblem
import math.max

case object Day2 extends AoCProblem[Int] {
  val day = 2

  private val input = scala.io.Source.fromFile("./inputs/2023/day2/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day2/test_input.txt").getLines().toSeq

  private val nRed = 12
  private val nGreen = 13
  private val nBlue = 14

  case class RGB(r: Int, g: Int, b: Int)
  case class Game(gameId: Int, draws: Seq[RGB])

  def combineRGB(left: RGB, right: RGB): RGB =
    RGB(max(left.r, right.r), max(left.g, right.g), max(left.b, right.b))

  def parseRow(row: String): Game = {
    val split = row.split(":").toSeq
    val gameId = split.head.split(" ").last.toInt
    val game = split.last
    val rbg = game.trim.split(";").toSeq.map { draw =>
      val blue = "\\d+ blue".r.findFirstIn(draw).map(_.split(" ").head.toInt).getOrElse(0)
      val red = "\\d+ red".r.findFirstIn(draw).map(_.split(" ").head.toInt).getOrElse(0)
      val green = "\\d+ green".r.findFirstIn(draw).map(_.split(" ").head.toInt).getOrElse(0)
      RGB(r = red, g = green, b = blue)
    }
    Game(gameId, rbg)
  }

  def solve1 = {
    input
      .map(parseRow)
      .filter{_.draws.forall { _ match { case RGB(r,g,b) => b <= nBlue && r <= nRed && g <= nGreen}}}
      .map(_.gameId)
      .sum
  }

  def solve2 = {
    input
      .map(parseRow)
      .map(_.draws)
      .map(draws => draws.tail.fold(draws.head)(combineRGB))
      .map(rgb => rgb.r * rgb.b * rgb.g)
      .sum
  }
}
