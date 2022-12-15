import scala.io.Source
import scala.annotation.tailrec

val fileName = "/home/uchan/code/fun/aoc2022/day14/src/main/resources/input_sample"
val input = Source
  .fromFile(fileName)
  .getLines
  .toList

// val testCase = input
val rocks = input
  .map(_.split("->").toList.map(_.trim))
  .map { (l: List[String]) =>
    for s <- l yield s match
      case s"$x,$y" => Coordinate(x.toInt, y.toInt)
      case _        => throw new Exception(s"value $s is not valid input")
  }

case class Coordinate(x: Int, y: Int)
type Canvas = Vector[Vector[Char]]

val xmin = rocks.map(innerList => innerList.map(_.x).min).min
val xmax = rocks.map(innerList => innerList.map(_.x).max).max
val ymin = 0
val ymax = rocks.map(innerList => innerList.map(_.y).max).max

val sand = Coordinate(500 - xmin, 0)

val canvas = Vector.fill(ymax - ymin + 1, xmax - xmin + 1)('.')

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

printCanvas(canvasWithRocks, sand)

def fillCanvasWithRocks(canvas: Canvas, range: List[Int], xy: Int, direction: String): Canvas =
  println(s"Got $range and $xy")
  direction match
    case "up"    => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "down"  => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "right" => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case "left"  => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case _       => throw new Exception(s"value $direction is not valid")

def printCanvas(canvas: Canvas, sand: Coordinate): Unit =
  val cvs = updateCanvas(canvas, sand, '+')
  println(cvs.map(_.mkString("")).mkString("\n"))

def initialFallingSpot(canvas: Canvas, sand: Coordinate): Coordinate =
  val ys = canvas.map(_(sand.x)).zipWithIndex.filter(_._1 == '#').head._2
  Coordinate(sand.x, ys - 1)

def bounds = (rowLength: Int, colLength: Int) =>
  (drop: Coordinate) =>
    println(s"row: $rowLength. col: $colLength")
    if drop.x == -1 ||
      drop.y == -1 ||
      drop.x == colLength ||
      drop.y == rowLength
    then true
    else false
    // then None
    // else Some(drop)

def checkBounds = bounds(xmax - xmin, ymax)

def finish = (sandSource: Coordinate) =>
  (fallingSpot: Coordinate) => if fallingSpot == Coordinate(sandSource.x, 1) then true else false
def checkIsFinished = finish(sand)

def countSands(canvas: Canvas): Unit =
  // TODO. change this later to 'o' later
  val ss = canvas.map(_.filter(_ == '#').length).sum
  println(ss)
  ()

def getFromCanvas(canvas: Canvas, c: Coordinate): Char = canvas(c.y)(c.x)
// def getFromCanvas(canvas: Canvas, c: Coordinate): Char = canvas(c.y)(c.x)
def updateCanvas(canvas: Canvas, c: Coordinate, item: Char): Canvas =
  canvas.updated(c.y, canvas(c.y).updated(c.x, item))

getFromCanvas(canvasWithRocks, Coordinate(9, 5))
getFromCanvas(canvasWithRocks, Coordinate(5, 9))
getFromCanvas(canvasWithRocks, Coordinate(5, 8))

// TODO: buggy
// @tailrec
// final def simulateSandDrop(
//     canvas: Canvas,
//     currentDrop: Coordinate,
//     fallingSpot: Coordinate
// ): Canvas =
//   if checkIsFinished(fallingSpot) then
//     println("Finished with these canvas")
//     println(canvas)
//     canvas
//   else
//     val ahead      = Coordinate(fallingSpot.x, fallingSpot.y + 1)
//     val leftAhead  = Coordinate(fallingSpot.x - 1, fallingSpot.y + 1)
//     val rightAhead = Coordinate(fallingSpot.x + 1, fallingSpot.y + 1)
//     if checkBounds(ahead) then
//       println(s"Rest case by bounds at $currentDrop")
//       val updatedCanvas  = updateCanvas(canvas, currentDrop, 'o')
//       val newFallingSpot = if currentDrop == fallingSpot then fallingSpot.copy(y = fallingSpot.y - 1) else fallingSpot
//       simulateSandDrop(updatedCanvas, newFallingSpot, newFallingSpot)
//     else if getFromCanvas(canvas, ahead) == '.' then simulateSandDrop(canvas, ahead, fallingSpot)
//     else if !checkBounds(leftAhead) && getFromCanvas(canvas, leftAhead) == '.' then
//       simulateSandDrop(canvas, leftAhead, fallingSpot)
//     else if !checkBounds(rightAhead) && getFromCanvas(canvas, rightAhead) == '.' then
//       simulateSandDrop(canvas, rightAhead, fallingSpot)
//     else
//       println(s"Rest case at $currentDrop")
//       val updatedCanvas  = updateCanvas(canvas, currentDrop, 'o')
//       val newFallingSpot = if currentDrop == fallingSpot then fallingSpot.copy(y = fallingSpot.y - 1) else fallingSpot
//       simulateSandDrop(updatedCanvas, newFallingSpot, newFallingSpot)

// val initialFall = initialFallingSpot(canvasWithRocks, sand)
// val result      = simulateSandDrop(canvasWithRocks, initialFall, initialFall)

// printCanvas(result, sand)

// ! Unused
// def detectCollision(canvas: Canvas, drop: Coordinate): Boolean =
//   val (x, y) = drop
//   if canvas(y)(x) != '.' then detectCollision(canvas, (x - 1, y))
//   else if canvas(y)(x - 1) != '.' then detectCollision(canvas, (x + 1, y))
//   else if canvas(y)(x + 1) != '.' then detectCollision(canvas, (x, y + 1))
//   else Some((x, y + 1))

// @tailrec
// def simulateSandDrop(canvas: Canvas, sandSource: Coordinate, lastDrop: Coordinate = (-1, -1)): Canvas =
//   if canvas(sandSource.y)(sandSource.x + 2) == 'o'
//     && canvas(sandSource.y)(sandSource.x + 1) == 'o'
//   then
//     println("Stop!")
//     canvas
//   else if lastDrop == (-1, -1) then
//     println("First time drop!")
//     val drop = (sandSource.x, 1)
//     simulateSandDrop(canvas, sandSource, drop)
//   else
//     val (x, y) = lastDrop
//     if canvas(y)(x) != '.' then

//     simulateSandDrop(canvas, sandSource)

// else if lastDrop == (-1, -1) then
//   println("First time drop!")
//   val xs   = canvas.map(_(sandSource.y))
//   val drop = (sandSource.x, xs.indexOf('#') - 1)
//   val uc   = canvas.updated(drop.y, canvas(drop.y).updated(drop.x, 'o'))
//   simulateSandDrop(canvas, sandSource, drop)
// else
//   val (x, y) = lastDrop
//   if canvas(x - 1)(y) == '.' then
//     println(s"Drop left: $lastDrop")
//     // canvas.updated(y, canvas(y).updated(x - 1, 'o'))
//     val drop = if (sandSource.x - (x - 1)).abs == 2 then (sandSource.x, y) else (x - 1, y)
//     simulateSandDrop(canvas, sandSource, drop)
//   else if canvas(x + 1)(y) == '.' then
//     println(s"Drop right: $lastDrop")
//     // canvas.updated(y, canvas(y).updated(x + 1, 'o'))
//     val drop = (x + 1, y)
//     simulateSandDrop(canvas, sandSource, drop)
//   else if canvas(x)(y - 1) == '.' then
//     println(s"Drop top: $lastDrop")
//     // canvas.updated(y - 1, canvas(y - 1).updated(x, 'o'))
//     val drop = (x, y - 1)
//     simulateSandDrop(canvas, sandSource, drop)
// ()

// simulateSandDrop(canvasWithRocks, sand)
