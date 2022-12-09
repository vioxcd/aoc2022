package day9

import org.scalatest.funsuite.AnyFunSuite

class Day9Suite extends AnyFunSuite {
  val testInput = List(
    Direction("R", 4),
    Direction("U", 4),
    Direction("L", 3),
    Direction("D", 1),
    Direction("R", 4),
    Direction("D", 1),
    Direction("L", 5),
    Direction("R", 2)
  )

  test("Number of positions that the tail visited at least once = 13") {
    assert(part1(testInput) == 13)
  }
}
