package day14

import scala.io.Source
import scala.annotation.tailrec

@main def main: Unit =
  val sampleCase = inputFileLoader("/input_sample")
  // val testCase   = inputFileLoader("/input")

  val pt1Sample = part1(sampleCase)
  // val pt1Test   = part1(testCase)

  println("--- First case ---")
  println(s"sample: $pt1Sample")
  // println(s"actual: $pt1Test")

  // val pt2Sample = part2(sampleCase)
  // val pt2Test   = part2(testCase)

  // println("\n--- Second input ---")
  // println(s"sample: $pt2Sample")
  // println(s"actual: $pt2Test")

case class Coordinate(x: Int, y: Int)
type Canvas = Vector[Vector[Char]]

def fillCanvasWithRocks(canvas: Canvas, range: List[Int], xy: Int, direction: String): Canvas =
  println(s"Got $range and $xy")
  direction match
    case "up"    => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "down"  => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "right" => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case "left"  => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case _       => throw new Exception(s"value $direction is not valid")

def initialFallingSpot(canvas: Canvas, sand: Coordinate): Coordinate =
  val ys = canvas.map(_(sand.x)).zipWithIndex.filter(_._1 == '#').head._2
  Coordinate(sand.x, ys - 1)

def bounds = (rowLength: Int, colLength: Int) =>
  (drop: Coordinate) =>
    // println(s"row: $rowLength. col: $colLength")
    if drop.x == -1 ||
      drop.y == -1 ||
      drop.x == colLength ||
      drop.y == rowLength
    then true
    else false
    // then None
    // else Some(drop)

def finish = (sandSource: Coordinate) =>
  (fallingSpot: Coordinate) => if fallingSpot == Coordinate(sandSource.x, 0) then true else false

def countSands(canvas: Canvas): Unit =
  // TODO. change this later to 'o' later
  val ss = canvas.map(_.filter(_ == '#').length).sum
  println(ss)
  ()

def getFromCanvas(canvas: Canvas, c: Coordinate): Char = canvas(c.y)(c.x)
def updateCanvas(canvas: Canvas, c: Coordinate, item: Char): Canvas =
  canvas.updated(c.y, canvas(c.y).updated(c.x, item))

def part1(input: List[String]): Unit =
  val rocks = input
    .map(_.split("->").toList.map(_.trim))
    .map { (l: List[String]) =>
      for s <- l yield s match
        case s"$x,$y" => Coordinate(x.toInt, y.toInt)
        case _        => throw new Exception(s"value $s is not valid input")
    }

  val xmin   = rocks.map(innerList => innerList.map(_.x).min).min
  val xmax   = rocks.map(innerList => innerList.map(_.x).max).max
  val ymin   = 0
  val ymax   = rocks.map(innerList => innerList.map(_.y).max).max
  val sand   = Coordinate(500 - xmin, 0)
  val canvas = Vector.fill(ymax - ymin + 1, xmax - xmin + 1)('.')

  // println(s"xmin: $xmin")
  // println(s"xmax: $xmax")
  // println(s"ymin: $ymin")
  // println(s"ymax: $ymax")

  def isBounded  = bounds(xmax - xmin, ymax)
  def isFinished = finish(sand)

  val canvasWithRocks = rocks
    .map(_.sliding(2))
    .flatten
    .foldLeft(canvas) { (acc, range) =>
      val from :: to :: _ = range
      val fromNorm        = Coordinate(from.x - xmin, from.y)
      val toNorm          = Coordinate(to.x - xmin, to.y)
      (fromNorm, toNorm) match
        case (c, n) if c.x == n.x && c.y < n.y => fillCanvasWithRocks(acc, (c.y to n.y).toList, c.x, "up")
        case (c, n) if c.x == n.x && c.y > n.y => fillCanvasWithRocks(acc, (n.y to c.y).toList, c.x, "down")
        case (c, n) if c.y == n.y && c.x < n.x => fillCanvasWithRocks(acc, (c.x to n.x).toList, c.y, "right")
        case (c, n) if c.y == n.y && c.x > n.x => fillCanvasWithRocks(acc, (n.x to c.x).toList, c.y, "left")
        case (_, _)                            => throw new Exception(s"value $from and $to is not valid")
    }

  def canvasPrinter = (sand: Coordinate) =>
    (canvas: Canvas) =>
      val cvs = updateCanvas(canvas, sand, '+')
      println(cvs.map(_.mkString("")).mkString("\n"))
  val printCanvas = canvasPrinter(sand)

  @tailrec
  def simulateSandDrop(
      canvas: Canvas,
      currentDrop: Coordinate,
      fallingSpot: Coordinate
  ): Canvas =
    println(s"Processing $currentDrop")
    // printCanvas(canvas)
    if isFinished(fallingSpot) then
      println("Finished with these canvas")
      printCanvas(canvas)
      canvas
    else
      val ahead      = Coordinate(currentDrop.x, currentDrop.y + 1)
      val leftAhead  = Coordinate(currentDrop.x - 1, currentDrop.y + 1)
      val rightAhead = Coordinate(currentDrop.x + 1, currentDrop.y + 1)
      if isBounded(ahead) then
        println(s"Rest case by bounds at $currentDrop")
        val updatedCanvas  = updateCanvas(canvas, currentDrop, 'o')
        val newFallingSpot = if currentDrop == fallingSpot then fallingSpot.copy(y = fallingSpot.y - 1) else fallingSpot
        simulateSandDrop(updatedCanvas, newFallingSpot, newFallingSpot)
      else if getFromCanvas(canvas, ahead) == '.' then
        println(s"Going ahead! $ahead")
        simulateSandDrop(canvas, ahead, fallingSpot)
      else if !isBounded(leftAhead) && getFromCanvas(canvas, leftAhead) == '.' then
        println(s"Going left ahead! $leftAhead")
        simulateSandDrop(canvas, leftAhead, fallingSpot)
      else if !isBounded(rightAhead) && getFromCanvas(canvas, rightAhead) == '.' then
        println(s"Going right  ahead! $rightAhead")
        simulateSandDrop(canvas, rightAhead, fallingSpot)
      else
        println(s"Rest case at $currentDrop")
        val updatedCanvas  = updateCanvas(canvas, currentDrop, 'o')
        val newFallingSpot = if currentDrop == fallingSpot then fallingSpot.copy(y = fallingSpot.y - 1) else fallingSpot
        simulateSandDrop(updatedCanvas, newFallingSpot, newFallingSpot)

  printCanvas(canvasWithRocks)
  val initialFall = initialFallingSpot(canvasWithRocks, sand)
  simulateSandDrop(canvasWithRocks, initialFall, initialFall)
  ()

def part2(data: List[String]): Int =
  15

def inputFileLoader(filename: String): List[String] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .toList
