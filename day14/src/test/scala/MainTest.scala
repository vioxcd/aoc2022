package day14

import org.scalatest.funsuite.AnyFunSuite

class Day14Suite extends AnyFunSuite {
  val testInput = List(
    "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9"
  )

  test("The amount of sand that comes to rest are 24") {
    assert(part1(testInput) == 24)
  }
}
