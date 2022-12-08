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

  val pt2Sample = part2(sampleCase)
  val pt2Test   = part2(testCase)

  println("\n--- Second input ---")
  println(s"sample: $pt2Sample")
  println(s"actual: $pt2Test")

def inputFileLoader(filename: String): Vector[Vector[Int]] =
  // return type of this function is modified and customized for each day's input
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .map(_.map(_.asDigit).toVector)
    .toVector

case class NWSETrees(
    treeHeight: Int,
    NWSE: Vector[Vector[Int]]
)

def getEdgesCount(tiles: Vector[Vector[Int]]): Int =
  val colLength = tiles(0).length
  val rowLength = tiles.length
  ((colLength - 2) * 2) + (rowLength * 2)

def getNWSETrees(tiles: Vector[Vector[Int]]): IndexedSeq[NWSETrees] =
  val colLength = tiles(0).length
  val rowLength = tiles.length

  for
    i <- (1 until colLength - 1)
    j <- (1 until rowLength - 1)
  yield
    val column        = tiles.map(_(j))
    val row           = tiles(i)
    val currentHeight = tiles(i)(j)

    val N = column.slice(0, i).reverse // from tree to top
    val W = row.slice(0, j).reverse    // from tree to left
    val S = column.slice(i + 1, rowLength)
    val E = row.slice(j + 1, colLength)

    NWSETrees(currentHeight, Vector(N, W, S, E))

def calculateTreesVisibility(nwseTrees: IndexedSeq[NWSETrees]): IndexedSeq[Boolean] =
  // return a list of visible trees (negate the result of non-visibility)
  for currentTree <- nwseTrees
  yield !currentTree.NWSE
    .map(trees => trees.exists(_ >= currentTree.treeHeight)) // exist a tree >= current tree height
    .forall(_ == true) // if true for all, then it's not visible!

def calculateScenicScore(nwseTrees: IndexedSeq[NWSETrees]): Int =
  def checkBounds(computed: Vector[Int], source: Vector[Int]): Int =
    // if length is the same as computed, don't add 1 (don't count last "blocker" tree)
    if computed.length == source.length then computed.length else computed.length + 1

  val scenicScores =
    for currentTree <- nwseTrees
    yield currentTree.NWSE
      .map(trees => (trees, trees.takeWhile(_ < currentTree.treeHeight))) // while height
      .map((source, transformed) => checkBounds(transformed, source))
      .reduce(_ * _)
  scenicScores.max

def part1(tiles: Vector[Vector[Int]]): Int =
  val edgeItemsCount     = getEdgesCount(tiles)
  val nwseTrees          = getNWSETrees(tiles)
  val treeVisibilityList = calculateTreesVisibility(nwseTrees)
  treeVisibilityList
    .groupBy(identity)
    .mapValues(_.size)(true) + edgeItemsCount // return the visible ones

def part2(tiles: Vector[Vector[Int]]): Int =
  val edgeItemsCount = getEdgesCount(tiles)
  val nwseTrees      = getNWSETrees(tiles)
  calculateScenicScore(nwseTrees)
