package day13

import org.scalatest.funsuite.AnyFunSuite

class Day13Suite extends AnyFunSuite {
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

  val testInputLarger = List(
    Direction("R", 5),
    Direction("U", 8),
    Direction("L", 8),
    Direction("D", 3),
    Direction("R", 17),
    Direction("D", 10),
    Direction("L", 25),
    Direction("U", 20)
  )

  test("Number of positions that the tail visited at least once = 13") {
    assert(part1(testInput) == 13)
  }

  test("Number of positions that the tail visited at least once in larger input (10 knot) = 36") {
    assert(part2(testInputLarger) == 36)
  }
}
