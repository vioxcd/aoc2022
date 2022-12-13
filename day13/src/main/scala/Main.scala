package day13

import scala.io.Source

@main def main: Unit =
  val sampleCase = inputFileLoader("/input_sample")
  // val testCase   = inputFileLoader("/input")

  val pt1Sample = part1(sampleCase)
  // val pt1Test   = part1(testCase)

  println("--- First case ---")
  println(s"sample: $pt1Sample")
  // println(s"actual: $pt1Test")

  // val pt2Sample = part2(sampleCase)
  // val pt2Test = part2(testCase)

  // println("\n--- Second input ---")
  // println(s"sample: $pt2Sample")
  // println(s"actual: $pt2Test")

def part1(s: List[String]): Int =
  15

def inputFileLoader(filename: String): List[String] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .toList
