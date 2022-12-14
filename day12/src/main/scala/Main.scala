package day12

import scala.io.Source
import scala.annotation.tailrec

@main def main: Unit =
  val sampleCase = inputFileLoader("/input_sample")
  val testCase   = inputFileLoader("/input")

  val pt1Sample = part1(sampleCase)
  val pt1Test   = part1(testCase)

  println("--- First case ---")
  println(s"sample: $pt1Sample")
  println(s"actual: $pt1Test")

  // val pt2Sample = part2(sampleCase)
  // val pt2Test   = part2(testCase)

  // println("\n--- Second input ---")
  // println(s"sample: $pt2Sample")
  // println(s"actual: $pt2Test")

type Grid = Vector[Vector[Char]]
case class Coordinate(x: Int, y: Int)
case class Step(c: Coordinate, count: Int)

def generatePathways(coord: Coordinate): List[Coordinate] =
  List(
    Coordinate(coord.x - 1, coord.y),
    Coordinate(coord.x + 1, coord.y),
    Coordinate(coord.x, coord.y - 1),
    Coordinate(coord.x, coord.y + 1)
  )

def bounds = (rowLength: Int, colLength: Int) =>
  (coord: Coordinate) =>
    if coord.x == -1 ||
      coord.y == -1 ||
      coord.x == colLength ||
      coord.y == rowLength
    then None
    else Some(coord)

def move = (grid: Grid) =>
  (current: Coordinate, target: Coordinate) =>
    if (grid(target.y)(target.x) - grid(current.y)(current.x)) <= 1 then Some(target) else None

def paths = (grid: Grid) =>
  (coord: Coordinate) =>
    val colLength   = grid(0).length
    val rowLength   = grid.length
    def checkBounds = bounds(rowLength, colLength)
    def checkMove   = move(grid)
    def getPossiblePaths = generatePathways(coord)
      .map(checkBounds)
      .flatten
      .map(checkMove(coord, _))
      .flatten
    getPossiblePaths

def prep(data: List[String]): (Grid, Coordinate, Coordinate) =
  val input: Grid = data
    .map(_.split("").map(_.charAt(0)).toVector)
    .toVector

  val start = input.zipWithIndex.flatMap { case (e, i) =>
    if e.indexOf('S') != -1 then Some(Coordinate(e.indexOf('S'), i)) else None
  }.head

  val end = input.zipWithIndex.flatMap { case (e, i) =>
    if e.indexOf('E') != -1 then Some(Coordinate(e.indexOf('E'), i)) else None
  }.head

  val tmp = input
    .updated(start.y, input(start.y).updated(start.x, 'a'))
  val testCase = tmp
    .updated(end.y, tmp(end.y).updated(end.x, 'z'))

  (testCase, start, end)

@annotation.tailrec
def traverse(
    q: Seq[Step],
    v: Set[Coordinate],
    accumulator: Map[Coordinate, Int],
    computePossiblePaths: Coordinate => List[Coordinate]
): Map[Coordinate, Int] =
  if q.isEmpty then accumulator
  else
    val next          = q.head
    val possiblePaths = computePossiblePaths(next.c)

    val possiblePathsCosts = possiblePaths
      .map(Step(_, next.count + 1))
      .toSet

    val checkMinCost = for path <- possiblePathsCosts yield
      val costToPossiblePath = accumulator.getOrElse(path.c, 9999)
      if path.count < costToPossiblePath then Some(path)
      else None

    val succ       = checkMinCost.flatten.filterNot(q.contains(_))
    val updatedAcc = accumulator + (next.c -> next.count)

    if succ.isEmpty then traverse(q.tail, v + next.c, updatedAcc, computePossiblePaths)
    else traverse(q.tail ++ succ, v + next.c, updatedAcc, computePossiblePaths)

def traverseFrom(initial: Step, computePossiblePaths: Coordinate => List[Coordinate]) =
  traverse(Seq(initial), Set.empty, Map(initial.c -> 0), computePossiblePaths)

def part1(data: List[String]): Int =
  val (grid, start, end) = prep(data)

  println(s"Start: $start")
  println(s"End: $end")

  val getPossiblePaths = paths(grid)
  val res              = traverseFrom(Step(start, 0), getPossiblePaths)
  res(end)

// def part2(data: List[String]): Int =
//   val (grid, start, end) = prep(data)
//   val res                = run(grid, start, end)

//   // consider all a
//   val allAs = for
//     y <- 0 to grid.length - 1
//     x <- 0 to grid(0).length - 1
//     if grid(y)(x) == 'a'
//   yield
//     val current = Coordinate(x, y)
//     if res.isDefinedAt(current) then res(end) - res(current)
//   15

// val lowestElevation = res.foldLeft(0) { (highestCost, m) =>
//   val (k, v) = m
//   if grid(k.y)(k.x) == 'a' then if v > highestCost then v else highestCost
//   else highestCost
// }
// res(end) - lowestElevation

def inputFileLoader(filename: String): List[String] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .toList
