package aoc


object Main extends App {

  val problems: Seq[AoCProblem[_]] = Seq(
//    y23.Day1,
//    y23.Day2,
//    y23.Day3,
//    y23.Day4,
//    y23.Day5, // beware, it took 17 min to calculate Day 5 part 2
//    y23.Day6,
//    y23.Day7,
//    y23.Day8, // the answer for part 2 is dummy. See the comment in code
    y23.Day9
  )
  problems.foreach(_.printAnswers)
}
