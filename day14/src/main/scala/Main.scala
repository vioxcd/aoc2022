package day14

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

type Thing  = (Int, Int)
type Canvas = Vector[Vector[Char]]

def updateCanvas(canvas: Canvas, range: List[Int], xy: Int, direction: String): Canvas =
  // println(s"direction: $direction. got $range and $xy")
  direction match
    case "up"    => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "down"  => range.foldLeft(canvas)((acc, i) => acc.updated(i, acc(i).updated(xy, '#')))
    case "right" => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case "left"  => range.foldLeft(canvas)((acc, i) => acc.updated(xy, acc(xy).updated(i, '#')))
    case _       => throw new Exception(s"value $direction is not valid")

def printCanvas(canvas: Canvas, sand: Thing): Unit =
  val cvs = canvas.updated(sand._2, canvas(sand._2).updated(sand._1, '+'))
  println(cvs.map(_.mkString("")).mkString("\n"))

def simulateSandDrop(canvas: Canvas, sand: Thing): Unit =
  ()

def part1(input: List[String]): Unit =
  val rocks = input
    .map(_.split("->").toList.map(_.trim))
    .map { (l: List[String]) =>
      for s <- l yield s match
        case s"$x,$y" => (x.toInt, y.toInt)
        case _        => throw new Exception(s"value $s is not valid input")
    }

  val xmin = rocks.map(innerList => innerList.map(_._1).min).min
  val xmax = rocks.map(innerList => innerList.map(_._1).max).max
  val ymin = 0
  // val ymin = rocks.map(innerList => innerList.map(_._2).min).min
  val ymax = rocks.map(innerList => innerList.map(_._2).max).max

  // println(s"xmin: $xmin")
  // println(s"xmax: $xmax")
  // println(s"ymin: $ymin")
  // println(s"ymax: $ymax")

  val sand: Thing = (500 - xmin, 0)
  val canvas      = Vector.fill(ymax - ymin + 1, xmax - xmin + 1)('.')

  // println(s"canvas size: x: ${canvas(0).length} | y: ${canvas.length}")
  val canvasWithRocks = rocks
    .map(_.sliding(2))
    .flatten
    .foldLeft(canvas) { (acc, range) =>
      val from :: to :: _ = range
      val fromNorm        = (from._1 - xmin, from._2)
      val toNorm          = (to._1 - xmin, to._2)
      // println(s"normalized from: $fromNorm. normalized to: $toNorm")
      (fromNorm, toNorm) match
        case (c, n) if c._1 == n._1 && c._2 < n._2 => updateCanvas(acc, (c._2 to n._2).toList, c._1, "up")
        case (c, n) if c._1 == n._1 && c._2 > n._2 => updateCanvas(acc, (n._2 to c._2).toList, c._1, "down")
        case (c, n) if c._2 == n._2 && c._1 < n._1 => updateCanvas(acc, (c._1 to n._1).toList, c._2, "right")
        case (c, n) if c._2 == n._2 && c._1 > n._1 => updateCanvas(acc, (n._1 to c._1).toList, c._2, "left")
        case (_, _)                                => throw new Exception(s"value $from and $to is not valid")
    }

  printCanvas(canvasWithRocks, sand)
  ()

def part2(data: List[String]): Int =
  15

def inputFileLoader(filename: String): List[String] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .toList
