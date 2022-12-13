package day12

import org.scalatest.funsuite.AnyFunSuite

class Day12Suite extends AnyFunSuite {
  val testInput = List(
    "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi"
  )

  test("This path reaches the goal in 31 steps") {
    assert(part1(testInput) == 31)
  }
}
