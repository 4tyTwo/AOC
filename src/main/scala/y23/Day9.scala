package y23

import aoc.AoCProblem

case object Day9 extends AoCProblem[Int] {
  val day = 9

  private val input = scala.io.Source.fromFile("./inputs/2023/day9/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day9/test_input.txt").getLines().toSeq

  def getDerivatives(acc: Seq[Seq[Int]], input: Seq[Int]): Seq[Seq[Int]] = {
    if (input.forall(_ == 0)) {
      acc
    } else {
      val derivative = getDerivative(input)
      getDerivatives(acc.appended(derivative), derivative)
    }
  }

  def getDerivative(seq: Seq[Int]): Seq[Int] = {
    seq.tail.foldLeft((seq.head, Seq.empty[Int])) { case (acc, newVal) =>
      (newVal, acc._2.appended(newVal - acc._1))
    }._2
  }

  def extrapolate(derivatives: Seq[Seq[Int]]): Seq[Int] = {
    val tail = derivatives.reverse.tail
    tail.foldLeft((0, Seq.empty[Int])) { case (acc, seq) =>
      val appended = seq.appended(seq.last + acc._1)
      (appended.last, appended)
    }._2
  }

  def extrapolateBack(derivatives: Seq[Seq[Int]]): Seq[Int] = {
    val tail = derivatives.reverse.tail
    tail.foldLeft((0, Seq.empty[Int])) { case (acc, seq) =>
      val prepended = seq.prepended(seq.head - acc._1)
      (prepended.head, prepended)
    }._2
  }

  def solve1: Int = {
    input.map { s =>
      val line = s.split(' ').map(_.toInt)
      extrapolate(getDerivatives(Seq.empty, line).prepended(line)).last
    }.sum
  }

  def solve2: Int = {
    input.map { s =>
      val line = s.split(' ').map(_.toInt)
      extrapolateBack(getDerivatives(Seq.empty, line).prepended(line)).head
    }.sum
  }
}
