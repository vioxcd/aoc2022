package day6

import scala.io.Source

@main def main: Unit =
  val testCase =
    Source
      .fromFile(getClass.getResource("/input").getFile)
      .getLines
      .next

  println(s"--- First input ---")
  println(part1(testCase))

  println(s"--- Second input ---")
  println(part2(testCase))

def detectStart(s: String, windowSize: Int) =
  s.sliding(windowSize).takeWhile(_.toSet.size < windowSize).length + windowSize

def part1(s: String): Int =
  detectStart(s, 4)

def part2(s: String): Int =
  detectStart(s, 14)
