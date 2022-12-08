package day8

import org.scalatest.funsuite.AnyFunSuite

class Day8Suite extends AnyFunSuite {
  val testInput = Vector(
    Vector(3, 0, 3, 7, 3),
    Vector(2, 5, 5, 1, 2),
    Vector(6, 5, 3, 3, 2),
    Vector(3, 3, 5, 4, 9),
    Vector(3, 5, 3, 9, 0)
  )

  test("Number of visible trees are 16 + 5 = 21") {
    assert(part1(testInput) == 21)
  }

  test("Highest scenic score for the set of trees are 8") {
    assert(part2(testInput) == 8)
  }
}
