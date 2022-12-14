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
  // val pt2Test = part2(testCase)

  // println("\n--- Second input ---")
  // println(s"sample: $pt2Sample")
  // println(s"actual: $pt2Test")

type Grid = Vector[Vector[Char]]
case class Coordinate(x: Int, y: Int)
case class Step(c: Coordinate, count: Int)
case class State(q: List[Coordinate], v: Set[Coordinate])

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
// def checkBounds = bounds(rowLength, colLength)

def finish = (grid: Grid) => (coord: Coordinate) => if grid(coord.y)(coord.x) == 'E' then true else false
// def checkFinish = finish(input)

def move = (grid: Grid) =>
  (current: Coordinate, target: Coordinate) =>
    if (grid(target.y)(target.x) - grid(current.y)(current.x)) <= 1 then Some(target) else None
// def checkMove = move(input)

def part1(data: List[String]): Int =
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

  val colLength = testCase(0).length
  val rowLength = testCase.length
  val visited   = Set(start)
  val queue     = List(start)
  val state     = State(queue, visited)

  def checkBounds = bounds(rowLength, colLength)
  def checkFinish = finish(testCase)
  def checkMove   = move(testCase)

  println(s"Start: $start")
  println(s"End: $end")

  @annotation.tailrec
  def traverse(
      g: Grid,
      q: Seq[Step],
      v: Set[Coordinate],
      accumulator: Map[Coordinate, Int]
      // end: Coordinate
  ): Map[Coordinate, Int] =
    if q.isEmpty then accumulator
    else
      val next = q.head
      // if next == end then
      //   println("Finished!")
      //   accumulator :+ Step(next.c, next.count + 1)

      val possiblePaths = generatePathways(next.c)
        .map(checkBounds)
        .flatten
        .map(checkMove(next.c, _))
        .flatten
        .map(Step(_, next.count + 1))
        .toSet

      // .filterNot(q.contains(_))
      // .filterNot((c: Coordinate) => v(c))

      val checkMinCost = for path <- possiblePaths yield
        val costToPossiblePath = accumulator.getOrElse(path.c, 9999)
        if path.count < costToPossiblePath then
          println(s"Update ${path.count} from ${costToPossiblePath}")
          Some(path)
        else None

      val succ       = checkMinCost.flatten.filterNot(q.contains(_))
      val updatedAcc = accumulator + (next.c -> next.count)

      if succ.isEmpty then
        println("empty!")
        traverse(g, q.tail, v + next.c, updatedAcc)
        // traverse(g, q.tail, v + next.c, accumulator, end)
      // else traverse(g, q.tail ++ succ, v + next.c, accumulator + (next.c -> next.count), end)
      else traverse(g, q.tail ++ succ, v + next.c, updatedAcc)

  def traverseFrom(g: Grid, initial: Step) =
    traverse(g, Seq(initial), Set.empty, Map(initial.c -> 0))
    // traverse(g, Seq(initial), Set.empty, Seq.empty, end)

  val res = traverseFrom(input, Step(start, 0))
  res(end)

def inputFileLoader(filename: String): List[String] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .toList
