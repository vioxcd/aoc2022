package day7

import org.scalatest.funsuite.AnyFunSuite

class Day7Suite extends AnyFunSuite {
  val testInput = List(
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  )
    .map(Terminal.from)

  test("Sum of directories with a total size of at most 100000 equal 95437 (a & e)") {
    assert(part1(testInput) == 95437)
  }
}
