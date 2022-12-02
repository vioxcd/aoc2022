package day1

import scala.io.Source

object Day1 {
  case class Elf(elfno: Int, calories: Int)

  def run(): Unit =
    val input =
      Source
        .fromFile(getClass.getResource("/input1").getFile)
        .getLines

    val testCase =
      input
        .foldLeft[List[Elf]](List(Elf(0, 0))) { case (elves, line) =>
          val elf = line match
            case "" => Elf(elves.last.elfno + 1, 0) // don't count line break
            case _  => Elf(elves.last.elfno, line.toInt)

          elves :+ elf
        }

    println(s"--- First input ---")
    println(part1(testCase))

    println(s"\n--- Second input ---")
    println(part2(testCase))

  def caloriesPerElf(elves: List[Elf]): Iterable[Int] =
    // GroupMap extract calories per elfno, elfno -> List[calories...]
    elves
      .groupMap(_.elfno)(_.calories)
      .values
      .map(_.sum)

  def part1(elves: List[Elf]): Int =
    caloriesPerElf(elves).max

  def part2(elves: List[Elf]): Int =
    caloriesPerElf(elves).toList
      .sorted(Ordering[Int].reverse)
      .take(3)
      .sum
}
