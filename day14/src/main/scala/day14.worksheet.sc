import scala.io.Source
import scala.util.Sorting

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
      case s"$x,$y" => (x.toInt, y.toInt)
      case _        => throw new Exception(s"value $s is not valid input")
  }

// enum DrawTo:
//   case Left
//   case Right
//   case Up
//   case Down

// case class Draw(where: DrawTo, range: List[Int], static: Int)

type Thing  = (Int, Int)
type Canvas = Vector[Vector[Char]]
// type Canvas = Map[(Int, Int), Char]

val xmin = rocks.map(innerList => innerList.map(_._1).min).min
val xmax = rocks.map(innerList => innerList.map(_._1).max).max
val ymin = 0
val ymax = rocks.map(innerList => innerList.map(_._2).max).max

val sand: Thing = (500 - xmin, 0)

// val coordinates = for
//   i <- (xmin to xmax)
//   j <- (ymin to ymax)
// yield (i, j) -> '.'

// val canvas = coordinates.toMap

val canvas = Vector.fill(xmax - xmin + 1, ymax - ymin + 1)('.')

val updatedCanvas = rocks
  .map(_.sliding(2))
  .flatten
  .foldLeft(canvas) { (acc, range) =>
    val from :: to :: _ = range
    val fromNorm        = (from._1 - xmin, from._2)
    val toNorm          = (to._1 - xmin, to._2)
    (fromNorm, toNorm) match
      case (c, n) if c._1 == n._1 && c._2 < n._2 => updateCanvas(acc, (c._2 to n._2).toList, c._1, "up")
      case (c, n) if c._1 == n._1 && c._2 > n._2 => updateCanvas(acc, (n._2 to c._2).toList, c._1, "down")
      case (c, n) if c._2 == n._2 && c._1 < n._1 => updateCanvas(acc, (c._1 to n._1).toList, c._2, "right")
      case (c, n) if c._2 == n._2 && c._1 > n._1 => updateCanvas(acc, (n._1 to c._1).toList, c._2, "left")
      case (_, _)                                => throw new Exception(s"value $from and $to is not valid")
  }

def updateCanvas(canvas: Canvas, range: List[Int], xy: Int, direction: String): Canvas =
  println(s"Got $range and $xy")
  direction match
    case "up"    => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "down"  => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "right" => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case "left"  => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case _       => throw new Exception(s"value $direction is not valid")

def printCanvas(canvas: Canvas, sand: Thing): Unit =
  val cvs = canvas.updated(sand._2, canvas(sand._2).updated(sand._1, '+'))
  println(cvs.map(_.mkString("")).mkString("\n"))

printCanvas(updatedCanvas, sand)

// def printCanvas(canvas: Canvas, sand: Thing): Unit =
//   val cvs = canvas + (sand -> '+')
//   println(cvs.values.grouped(ymax - ymin + 1).map(_.mkString("")).mkString("\n"))

// printCanvas(canvas, sand)

// def draw(rangePair: (Thing, Thing), canvas: Canvas): Canvas =
// val drawer = rangePair match
//   case (c, n) if c._1 == n._1 && c._2 < n._2 => Draw(DrawTo.Up, (c._2 to n._2).toList, c._1)
//   case (c, n) if c._1 == n._1 && c._2 > n._2 => Draw(DrawTo.Down, (n._2 to c._2).toList, c._1)
//   case (c, n) if c._2 == n._2 && c._1 < n._2 => Draw(DrawTo.Right, (c._1 to n._1).toList, c._2)
//   case (c, n) if c._2 == n._2 && c._1 > n._1 => Draw(DrawTo.Left, (n._1 to c._1).toList, c._2)
// case (c, n) if c._1 == n._1 && c._2 < n._2 => Draw.Up((c._2 to n._2).toList, c._1)
// case (c, n) if c._1 == n._1 && c._2 > n._2 => Draw.Down((n._2 to c._2).toList, c._1)
// case (c, n) if c._2 == n._2 && c._1 < n._2 => Draw.Right((c._1 to n._1).toList, c._2)
// case (c, n) if c._2 == n._2 && c._1 > n._1 => Draw.Left((n._1 to c._1).toList, c._2)
// case _ => throw new Exception(s"value $rangePair is not valid")

// drawer.range.foldLeft(canvas) { (c, r) =>
//   canvas.updated()
// }

// rocks.foldLeft(canvas) { (c, rs) =>
//   for iter <- rs.sliding(2) yield canvas.updated()
// }

// def draw(rocks: List[List[(Int, Int)]], sand: (Int, Int)): Unit =
//   val xMin = rocks.map(_.min).min
//   val xMax = rocks.map(_.max).max
//   val yMin = rocks.map(innerList => innerList.map(_._2).min).min
//   val yMax = rocks.map(innerList => innerList.map(_._2).max).max
//   println(s"$xMin")
//   println(s"$xMax")

// draw(testCase, sand)
