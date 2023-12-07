package y23

import aoc.AoCProblem

import scala.collection.mutable
import scala.math.Ordering.Implicits.seqOrdering

case object Day7 extends AoCProblem[Int] {
  val day = 7

  private val input = scala.io.Source.fromFile("./inputs/2023/day7/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day7/test_input.txt").getLines().toSeq

  sealed trait HandType {
    val value: Int
  }

  case object FiveOfKind extends HandType  { val value = 6 }
  case object FourOfKind extends HandType  { val value = 5 }
  case object FullHouse extends HandType   { val value = 4 }
  case object ThreeOfKind extends HandType { val value = 3 }
  case object TwoPair extends HandType     { val value = 2 }
  case object OnePair extends HandType     { val value = 1 }
  case object HighCard extends HandType    { val value = 0 }


  case class Card(card: Char)(implicit power: Map[Char, Int]) extends Ordered[Card] {
    override def compare(that: Card): Int = power(this.card) compare power(that.card)
  }

  sealed trait Hand extends Ordered[Hand] {

    val hand: Seq[Card]

    override def compare(that: Hand): Int = {
      if (handType == that.handType) {
        hand compare that.hand
      } else handType.value compare that.handType.value
    }
    def handType: HandType
  }

  case class Hand1(input: Seq[Char]) extends Hand {
    implicit val power = Seq('A', 'K', 'Q', 'J', 'T' , '9', '8', '7', '6', '5', '4', '3', '2').zip((0 to 12).reverse).toMap

    val hand: Seq[Card] = input.map(Card(_))

    def handType: HandType = {
      val cardCounter: mutable.Map[Card, Int] = mutable.Map()
      val updateFun: Option[Int] => Option[Int] = a => a.map(_ + 1).orElse(Some(1))
      hand.foreach { c => cardCounter.updateWith(c) (updateFun) }

      val values = cardCounter.values.toSeq
      if (values.contains(5)) {
        FiveOfKind
      } else if (values.contains(4)) {
        FourOfKind
      } else if (values.contains(3) && values.contains(2)) {
        FullHouse
      } else if (values.contains(3)) {
        ThreeOfKind
      } else if (values.count(_ == 2) == 2) {
        TwoPair
      } else if (values.count(_ == 2) == 1) {
        OnePair
      } else HighCard
    }
  }

  case class Hand2(input: Seq[Char]) extends Hand {
    implicit val power = Seq('A', 'K', 'Q', 'T' , '9', '8', '7', '6', '5', '4', '3', '2', 'J').zip((0 to 12).reverse).toMap

    val hand: Seq[Card] = input.map(Card(_))

    def handType: HandType = {
      val cardCounter: mutable.Map[Card, Int] = mutable.Map()
      val updateFun: Option[Int] => Option[Int] = a => a.map(_ + 1).orElse(Some(1))
      hand.foreach { c => cardCounter.updateWith(c) (updateFun) }

      val jokers: Int = cardCounter.getOrElse(Card('J'), 0)
      val values = cardCounter.filter(a => a._1 != Card('J')).values.toSeq
      val max = if (values.nonEmpty) values.max else 0
      if (values.contains(5) || max + jokers == 5) {
        FiveOfKind
      } else if (values.contains(4) || max + jokers == 4) {
        FourOfKind
      } else if ((values.contains(3) && values.contains(2)) || (values.count(_ == 2) == 2 && jokers == 1) || (values.count(_ == 2) == 1 &&  jokers == 2)) {
        FullHouse
      } else if (values.contains(3) || max + jokers == 3) {
        ThreeOfKind
      } else if (values.count(_ == 2) == 2 || (values.count(_ == 2) == 1 && jokers == 1)) {
        TwoPair
      } else if (values.count(_ == 2) == 1 || jokers >= 1) {
        OnePair
      } else HighCard
    }
  }

  def parseLine(line: String): (Seq[Char], Int) = {
    val parts = line.split(" ")
    (parts.head.toCharArray.toSeq, parts.last.trim.toInt)
  }

  def solve1: Int = {
    val game = input.map(parseLine).map(a => (Hand1(a._1), a._2))
    val sortedGames = game.sortBy(_._1)
    sortedGames.map(_._2).zipWithIndex.map { case(v, i) => v * (i + 1)}.sum
  }

  def solve2: Int = {
    val game = input.map(parseLine).map(a => (Hand2(a._1), a._2))
    val sortedGames = game.sortBy(_._1)
    sortedGames.map(_._2).zipWithIndex.map { case(v, i) => v * (i + 1)}.sum
  }
}
