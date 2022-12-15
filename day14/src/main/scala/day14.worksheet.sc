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
      case (c, n) if c.x == n.x && c.y < n.y => updateCanvas(acc, (c.y to n.y).toList, c.x, "up")
      case (c, n) if c.x == n.x && c.y > n.y => updateCanvas(acc, (n.y to c.y).toList, c.x, "down")
      case (c, n) if c.y == n.y && c.x < n.x => updateCanvas(acc, (c.x to n.x).toList, c.y, "right")
      case (c, n) if c.y == n.y && c.x > n.x => updateCanvas(acc, (n.x to c.x).toList, c.y, "left")
      case (_, _)                            => throw new Exception(s"value $from and $to is not valid")
  }

printCanvas(canvasWithRocks, sand)

def updateCanvas(canvas: Canvas, range: List[Int], xy: Int, direction: String): Canvas =
  println(s"Got $range and $xy")
  direction match
    case "up"    => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "down"  => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "right" => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case "left"  => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case _       => throw new Exception(s"value $direction is not valid")

def printCanvas(canvas: Canvas, sand: Coordinate): Unit =
  val cvs = canvas.updated(sand.y, canvas(sand.y).updated(sand.x, '+'))
  println(cvs.map(_.mkString("")).mkString("\n"))

def initialFallingSpot(canvas: Canvas, sand: Coordinate): Coordinate =
  val ys = canvas.map(_(sand.x)).zipWithIndex.filter(_._1 == '#').head._2
  Coordinate(sand.x, ys - 1)

initialFallingSpot(canvasWithRocks, sand)

// def bounds = (rowLength: Int, colLength: Int) =>
//   (drop: Coordinate) =>
//     println(s"row: $rowLength. col: $colLength")
//     val (x, y) = drop
//     if x == -1 ||
//       y == -1 ||
//       x == colLength ||
//       y == rowLength
//     then None
//     else Some(drop)

// def checkBounds = bounds(xmax - xmin, ymax)

def finish = (sandSource: Coordinate) =>
  (fallingSpot: Coordinate) => if fallingSpot == Coordinate(sandSource.x, 1) then true else false
def checkIsFinished = finish(sand)

def countSands(canvas: Canvas): Unit =
  // TODO. change this later to 'o' later
  val ss = canvas.map(_.filter(_ == '#').length).sum
  println(ss)
  ()

// @tailrec
def simulateSandDrop(canvas: Canvas, currentDrop: Coordinate, fallingSpot: Coordinate): Unit =
  if checkIsFinished(fallingSpot) then
    println("Finished with these canvas")
    println(canvas)

  val ahead = (fallingSpot.x, fallingSpot.y + 1)
  // if
  ()

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
