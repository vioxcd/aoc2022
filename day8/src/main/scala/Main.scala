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
    N: Vector[Int],
    W: Vector[Int],
    S: Vector[Int],
    E: Vector[Int]
)

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

    NWSETrees(currentHeight, N, W, S, E)

def calculateTreesVisibility(nwseTrees: IndexedSeq[NWSETrees]): IndexedSeq[Boolean] =
  // return a list of visible trees (negate the result of non-visibility)
  for currentTree <- nwseTrees
  yield
    val notVisibleFromNorth = currentTree.N.exists(_ >= currentTree.treeHeight)
    val notVisibleFromWest  = currentTree.W.exists(_ >= currentTree.treeHeight)
    val notVisibleFromSouth = currentTree.S.exists(_ >= currentTree.treeHeight)
    val notVisibleFromEast  = currentTree.E.exists(_ >= currentTree.treeHeight)
    !(notVisibleFromNorth && notVisibleFromWest && notVisibleFromSouth && notVisibleFromEast)

def calculateScenicScore(nwseTrees: IndexedSeq[NWSETrees]): Int =
  def checkBounds(computed: Vector[Int], source: Vector[Int]): Int =
    // if length is the same as computed, don't add 1 (don't count last "blocker" tree)
    if computed.length == source.length then computed.length else computed.length + 1

  val scenicScores =
    for currentTree <- nwseTrees
    yield
      val treesInTheNorth = currentTree.N.takeWhile(_ < currentTree.treeHeight)
      val treesInTheWest  = currentTree.W.takeWhile(_ < currentTree.treeHeight)
      val treesInTheSouth = currentTree.S.takeWhile(_ < currentTree.treeHeight)
      val treesInTheEast  = currentTree.E.takeWhile(_ < currentTree.treeHeight)

      val northScenicScore = checkBounds(treesInTheNorth, currentTree.N)
      val westScenicScore  = checkBounds(treesInTheWest, currentTree.W)
      val southScenicScore = checkBounds(treesInTheSouth, currentTree.S)
      val eastScenicScore  = checkBounds(treesInTheEast, currentTree.E)

      northScenicScore * westScenicScore * southScenicScore * eastScenicScore
  scenicScores.max

def part1(tiles: Vector[Vector[Int]]): Int =
  val colLength      = tiles(0).length
  val rowLength      = tiles.length
  val edgeItemsCount = ((colLength - 2) * 2) + (rowLength * 2)

  val nwseTrees          = getNWSETrees(tiles)
  val treeVisibilityList = calculateTreesVisibility(nwseTrees)
  treeVisibilityList
    .groupBy(identity)
    .mapValues(_.size)(true) + edgeItemsCount // return the visible ones

def part2(tiles: Vector[Vector[Int]]): Int =
  val colLength      = tiles(0).length
  val rowLength      = tiles.length
  val edgeItemsCount = ((colLength - 2) * 2) + (rowLength * 2)

  val nwseTrees = getNWSETrees(tiles)
  calculateScenicScore(nwseTrees)
