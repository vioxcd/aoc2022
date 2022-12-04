package day4

import scala.io.Source

case class Pairs(p11: Int, p12: Int, p21: Int, p22: Int)

@main def main: Unit =
  val testCase =
    Source
      .fromFile(getClass.getResource("/input").getFile)
      .getLines
      .map(_.split(",").map(_.split("-").map(_.toInt)))
      .map(s => Pairs(s(0)(0), s(0)(1), s(1)(0), s(1)(1)))
      .toList

  println(s"--- First input ---")
  println(part1(testCase))

  println(s"--- Second input ---")
  println(part2(testCase))

def part1(pairs: List[Pairs]): Int =
  val intersectedPairs = pairs.map { pair =>
    val s1          = (pair.p11 to pair.p12).toSet
    val s2          = (pair.p21 to pair.p22).toSet
    val intersected = s1.intersect(s2)
    if s1.equals(intersected) || s2.equals(intersected) then 1 else 0
  }
  intersectedPairs.sum

def part2(pairs: List[Pairs]): Int =
  val intersectedPairs = pairs.map { pair =>
    val s1          = (pair.p11 to pair.p12).toSet
    val s2          = (pair.p21 to pair.p22).toSet
    val intersected = s1.intersect(s2)
    if intersected.size > 0 then 1 else 0
  }
  intersectedPairs.sum
