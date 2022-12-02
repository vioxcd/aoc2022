package day2

import org.scalatest.funsuite.AnyFunSuite

class Day2Suite extends AnyFunSuite {
  val testInput = Seq(
    "A Y",
    "B X",
    "C Z"
  )

  test(
    "Second column act as rock-paper-scissors strategy will give a score of 15"
  ) {
    assert(Main.part1(testInput) == 15)
  }

  test("Second column act as win-draw-lose strategy will give a score of 12") {
    assert(Main.part2(testInput) == 12)
  }
}
