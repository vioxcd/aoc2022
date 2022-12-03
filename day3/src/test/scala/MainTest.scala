package day3

import org.scalatest.funsuite.AnyFunSuite

class Day3Suite extends AnyFunSuite {
  val testInput = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )

  test("The sum of the priorities of items found in each rucksack are 157") {
    assert(Main.part1(testInput) == 157)
  }

  test(
    "The sum of the priorities of items that correspond to the badges of each three-Elf group is 70"
  ) {
    assert(Main.part2(testInput) == 70)
  }
}
