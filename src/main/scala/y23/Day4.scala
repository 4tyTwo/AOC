package y23

import aoc.AoCProblem
import scala.collection.mutable.Map

case object Day4 extends AoCProblem[Int] {
  val day = 4

  private val input = scala.io.Source.fromFile("./inputs/2023/day4/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day4/test_input.txt").getLines().toSeq


  case class Card(number: Int, winning: Set[Int], yours: Set[Int]) {
    def winningNumbers: Int = yours.intersect(winning).size
  }

  object Card {
    def apply(card: String): Card = {
      val split = card.split(":")
      val no = split.head.split(" +").last.toInt
      val numbers = split
        .last
        .split("\\|")
        .map(
          _
            .trim
            .split(" +")
            .map(_.toInt)
            .toSet
        )
      Card(no, numbers.head, numbers.last)
    }
  }

  def solve1: Int = {
    input
      .map(Card(_))
      .map(_.winningNumbers)
      .map( winningNumbers =>
        if (winningNumbers > 0) math.pow(2, winningNumbers - 1).toInt else 0
      ).sum
  }

  def solve2: Int = {
    val cards = input.map(Card(_))
    val cardPoints = cards.map(c => c.number -> c.winningNumbers).toMap
    val cardCounter = Map() ++ cards.map(c => c.number -> 1).toMap
    cardCounter.foreach { case (cardNo, count) =>
      val points = cardPoints(cardNo)
      if (points > 0) {
        (cardNo + 1 to cardNo + points).foreach { i =>
          val curr = cardCounter(i)
          cardCounter.update(i, count + curr)
        }
      }
    }
    cardCounter.values.sum
  }
}
