package day1

import org.scalatest.funsuite.AnyFunSuite
import day1.Main.Elf

// https://alvinalexander.com/scala/mill-build-tool/unit-testing-scalatest-dependencies/
class Day1Suite extends AnyFunSuite {
  val testInput = List(
    Elf(0, 0),
    Elf(0, 1000),
    Elf(0, 2000),
    Elf(0, 3000),
    Elf(1, 0),
    Elf(1, 4000),
    Elf(2, 0),
    Elf(2, 5000),
    Elf(2, 6000),
    Elf(3, 0),
    Elf(3, 7000),
    Elf(3, 8000),
    Elf(3, 9000),
    Elf(4, 0),
    Elf(4, 10000)
  )

  test("the Elf carrying the most Calories carries 24000") {
    assert(Main.part1(testInput) == 24000)
  }

  test("the sum of Calories that the top 3 Elfs are carriying are 45000") {
    assert(Main.part2(testInput) == 45000)
  }
}
