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
      case s"$x,$y" => (x.toInt, y.toInt)
      case _        => throw new Exception(s"value $s is not valid input")
  }

type Thing  = (Int, Int)
type Canvas = Vector[Vector[Char]]

val xmin = rocks.map(innerList => innerList.map(_._1).min).min
val xmax = rocks.map(innerList => innerList.map(_._1).max).max
val ymin = 0
val ymax = rocks.map(innerList => innerList.map(_._2).max).max

val sand: Thing = (500 - xmin, 0)

val canvas = Vector.fill(ymax - ymin + 1, xmax - xmin + 1)('.')

val canvasWithRocks = rocks
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

printCanvas(canvasWithRocks, sand)

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

// @tailrec
def simulateSandDrop(canvas: Canvas, sandSource: Thing, lastDrop: Thing = (-1, -1)): Unit =
  // if lastDrop == (sandSource._1 - 2, sandSource._2) then
  //   println("Stop!")
  //   canvas
  // else if lastDrop == (-1, -1) then
  //   println("First time drop!")
  //   val xs = canvas.map(_(sandSource._2))
  //   // println(xs)
  //   // printCanvas(uc, sand)
  //   val drop = (sandSource._1, xs.indexOf('#') - 1)
  //   println(drop)
  //   val uc = canvas.updated(drop._2, canvas(drop._2).updated(drop._1, 'o'))
  //   simulateSandDrop(canvas, sandSource, drop)
  // else
  //   val (x, y) = lastDrop
  //   if canvas(x - 1)(y) == '.' then
  //     println(s"Drop left: $lastDrop")
  //     // canvas.updated(y, canvas(y).updated(x - 1, 'o'))
  //     val drop = if (sandSource._1 - (x - 1)).abs == 2 then (sandSource._1, y) else (x - 1, y)
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
  ()

simulateSandDrop(canvasWithRocks, sand)
