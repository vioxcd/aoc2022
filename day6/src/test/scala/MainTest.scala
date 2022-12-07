package day6

import org.scalatest.funsuite.AnyFunSuite

case class Expected(input: String, part1: Int, part2: Int)

class Day6Suite extends AnyFunSuite {
  val testInput = List(
    Expected("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
    Expected("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
    Expected("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
    Expected("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
    Expected("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
  )

  test("The input values from the datastream buffer (part1: packet) must equal expected") {
    for input <- testInput
    do assert(part1(input.input) == input.part1)
  }

  test("The input values from the datastream buffer (part2: messages) must equal expected") {
    for input <- testInput
    do assert(part2(input.input) == input.part2)
  }
}
