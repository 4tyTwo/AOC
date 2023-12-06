package y23

import aoc.AoCProblem

case object Day5 extends AoCProblem[Long] {
  val day = 5

  private val input = scala.io.Source.fromFile("./inputs/2023/day5/input.txt").getLines().toSeq
  private val testInput = scala.io.Source.fromFile("./inputs/2023/day5/test_input.txt").getLines().toSeq

  case class MapRange(destination: Long, source: Long, length: Long) {
    val offset = destination - source

    def map(a: Long): Option[Long] =
      if (a <= source + length - 1 && a >= source) {
        Some(a + offset)
      } else None
  }

  def chunkMultiline(input: Seq[String]): Seq[Seq[String]] = {
    var result = Seq.empty[Seq[String]]
    var curr = Seq.empty[String]
    input.indices.foreach { i =>
      if (input(i).isBlank) {
        result = result.appended(curr)
        curr = Seq.empty
      } else {
        curr = curr.appended(input(i))
      }
    }
    if (curr.nonEmpty) result.appended(curr) else result
  }

  def parseInput(input: Seq[String]): (Seq[Long], Seq[Long => Long]) = {
    val seeds = input.head.split(": ").last.trim.split(" ").map(_.toLong)

    val maps = chunkMultiline(input.tail.tail) // input w/o first 2 lines
    val mappers = maps.map(a => parseMap(a.tail))
    (seeds, mappers)
  }

  def parseMap(mapDescription: Seq[String]): (Long) => Long = {
    val ranges = mapDescription.map { line =>
      val definition = line.split(" ").toSeq.map(_.toLong)
      MapRange(definition.head, definition(1), definition(2))
    }

    def mapper(a: Long): Long = {
      ranges.foldLeft(Option.empty[Long]) { case (option, range) =>
        if (option.isDefined) option else range.map(a)
      }.getOrElse(a)
    }
    mapper
  }

  def solve1: Long = {
    val (seeds, mappings) = parseInput(testInput)
    val locations = seeds.map(mappings.foldLeft(_){ case (i, mapper) =>
      mapper(i)
    })
    locations.min
  }

  case class MyRange(start: Long, length: Long) { // includes start, excludes start+length
    val end = start + length - 1
  }

  object MyRange {
    def apply(arr: Array[Long]): MyRange = {
      MyRange(arr(0), arr(1))
    }
  }

  def solve2: Long = {
    val (seeds, mappings) = parseInput(input)
    var i = 1;
    val locations = seeds
      .grouped(2)
      .map(group => {
        println(s"Calculating $i")
        val minOfRange = (group.head until group.head + group(1)).view.map(mappings.foldLeft(_) { case (i, mapper) =>
          mapper(i)
        }).min
        println(s"Finished with $i")
        i += 1
        minOfRange
      }
      )
    locations.min
  }
}
