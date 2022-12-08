package day8

import scala.io.Source

@main def main: Unit =
  val testCase =
    Source
      // .fromFile(getClass.getResource("/input").getFile)
      .fromFile(getClass.getResource("/input_sample").getFile)
      .getLines
      .map(_.map(_.asDigit).toVector)
      .toVector

  println(s"--- First input ---")
  println(part1(testCase))

  // println(s"--- Second input ---")
  // println(part2(testCase))

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
