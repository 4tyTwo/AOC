package aoc

trait AoCProblem[A] {

  val day: Int

  def solve1: A
  def solve2: A

  def printAnswers: Unit = {
    val (t1, r1) = time(solve1)
    val (t2, r2) = time(solve2)
    println(s"Day $day part 1: $r1, Elapsed time: $t1 μs")
    println(s"Day $day part 2: $r2, Elapsed time: $t2 μs")
  }

  // time in μs
  private def time[R](block: => R): (Long, R) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    ((t1 - t0) / 1000, result)
  }
}
