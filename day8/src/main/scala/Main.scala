package day8

import scala.io.Source

@main def main: Unit =
  val testCase   = inputFileLoader("/input")
  val sampleCase = inputFileLoader("/input_sample")

  val pt1Sample = part1(sampleCase)
  val pt1Test   = part1(testCase)

  println("--- First case ---")
  println(s"sample: $pt1Sample")
  println(s"actual: $pt1Test")

  // val pt2Sample = part2(sampleCase)
  // val pt2Test   = part2(testCase)

  // println("\n--- Second input ---")
  // println(s"sample: $pt1Sample")
  // println(s"actual: $pt1Test")

def inputFileLoader(filename: String): Vector[Vector[Int]] =
  // return type of this function is modified and customized for each day's input
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .map(_.map(_.asDigit).toVector)
    .toVector

def calculateNonVisibility(
    tiles: Vector[Vector[Int]],
    colLength: Int,
    rowLength: Int
): IndexedSeq[Boolean] =
  val res = for
    i <- (1 until colLength - 1)
    j <- (1 until rowLength - 1)
  yield
    val column  = tiles.map(_(j))
    val row     = tiles(i)
    val current = tiles(i)(j)

    // LTR
    val fromLeft           = row.slice(0, j)
    val notVisibleFromLeft = fromLeft.exists(_ >= current)

    // RTL
    val fromRight           = row.slice(j + 1, colLength)
    val notVisibleFromRight = fromRight.exists(_ >= current)

    // Top-down
    val fromTop           = column.slice(0, i)
    val notVisibleFromTop = fromTop.exists(_ >= current)

    // Bottom-up
    val fromBottom           = column.slice(i + 1, rowLength)
    val notVisibleFromBottom = fromBottom.exists(_ >= current)

    notVisibleFromLeft && notVisibleFromRight && notVisibleFromTop && notVisibleFromBottom
  res

def part1(tiles: Vector[Vector[Int]]): Int =
  val colLength      = tiles(0).length
  val rowLength      = tiles.length
  val edgeItemsCount = ((colLength - 2) * 2) + (rowLength * 2)

  val nonVisibilityList = calculateNonVisibility(tiles, colLength, rowLength)
  nonVisibilityList
    .groupBy(identity)
    .mapValues(_.size)(false) + edgeItemsCount // return the visible ones
