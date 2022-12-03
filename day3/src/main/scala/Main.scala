package day3

import scala.io.Source

object Main {
  val priorityTable = (
    (('a' to 'z') ++ ('A' to 'Z')) zip (1 to 52)
  ).toMap

  def main(args: Array[String]): Unit =
    val testCase =
      Source
        .fromFile(getClass.getResource("/input").getFile)
        .getLines
        .toList

    println(s"--- First input ---")
    println(part1(testCase))
    part1(testCase)

    println(s"--- Second input ---")
    println(part2(testCase))
    part2(testCase)

  def part1(items: List[String]): Int =
    val priorities: List[Int] = for item <- items yield
      val (c1, c2) = item.splitAt(item.length / 2)
      val duplicate = c1.toSet.intersect(c2.toSet).head
      priorityTable(duplicate)
    priorities.sum

  def part2(items: List[String]): Int =
    val priorities: List[Int] = for threeItem <- items.sliding(3,3).toList yield
      val i1 :: i2 :: i3 :: _ = threeItem 
      val duplicate = i1.toSet.intersect(i2.toSet).intersect(i3.toSet).head
      priorityTable(duplicate)
    priorities.sum
}
