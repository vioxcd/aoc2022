package day9

import scala.io.Source

@main def main: Unit =
  val sampleCase       = inputFileLoader("/input_sample")
  val sampleCaseLarger = inputFileLoader("/input_sample_larger")
  val testCase         = inputFileLoader("/input")

  // println(sampleCase)

  // val pt1Sample = part1(sampleCase)
  // val pt1Test   = part1(testCase)

  // println("--- First case ---")
  // println(s"sample: $pt1Sample")
  // println(s"actual: $pt1Test")

  val pt2Sample = part2(sampleCase)
  val pt2Test   = part2(testCase)

  println("\n--- Second input ---")
  println(s"sample: $pt2Sample")
  println(s"actual: $pt2Test")

case class Direction(where: String, distance: Int)
case class Coordinate(x: Int, y: Int)

object Direction:
  def from(s: String): Direction =
    val where :: distance :: _ = s.split(" ").toList
    Direction(where, distance.toInt)

def calculatePositionDiff(head: Coordinate, tail: Coordinate): Coordinate =
  Coordinate(head.x - tail.x, head.y - tail.y)

def moveTail(tail: Coordinate, diff: Coordinate): Coordinate =
  Coordinate(tail.x + diff.x.sign, tail.y + diff.y.sign)

def calculateTailPosition(head: Coordinate, tail: Coordinate): Coordinate =
  val diff    = calculatePositionDiff(head, tail)
  val absDiff = Coordinate(diff.x.abs, diff.y.abs)

  absDiff match
    case Coordinate(0, 0) | Coordinate(1, 1) | Coordinate(1, 0) | Coordinate(0, 1) => tail // not behind
    case Coordinate(2, 0) | Coordinate(0, 2) | Coordinate(2, 1) | Coordinate(1, 2) | Coordinate(2, 2) =>
      moveTail(tail, diff)
    case Coordinate(_, _) => throw new Exception(s"position calculation went wrong: got $diff")

def calculateHeadTracks(directions: List[Direction], head: Coordinate): List[Coordinate] =
  directions.foldLeft(List(head)) { (accum, direction) =>
    val current = accum.takeRight(1)(0)
    val tracks = direction.where match
      case "R" => (1 to direction.distance).map(step => Coordinate(current.x + step, current.y)).toList
      case "L" => (1 to direction.distance).map(step => Coordinate(current.x - step, current.y)).toList
      case "U" => (1 to direction.distance).map(step => Coordinate(current.x, current.y + step)).toList
      case "D" => (1 to direction.distance).map(step => Coordinate(current.x, current.y - step)).toList
      case _   => throw new Exception(s"value $direction is not a valid direction")
    accum ::: tracks
  }

def calculateTailTracks(headTracks: List[Coordinate], tail: Coordinate): List[Coordinate] =
  headTracks.foldLeft(List(tail)) { (accum, headTrack) =>
    val current = accum.takeRight(1)(0)
    val tail    = calculateTailPosition(headTrack, current)
    accum :+ tail
  }

def part1(directions: List[Direction]): Int =
  val head       = Coordinate(0, 0)
  val tail       = Coordinate(0, 0)
  val headTracks = calculateHeadTracks(directions, head)
  val tailTracks = calculateTailTracks(headTracks, tail)
  tailTracks.toSet.size

def part2(directions: List[Direction]): Int =
  val head       = Coordinate(0, 0)
  val tail       = Coordinate(0, 0)
  val headTracks = calculateHeadTracks(directions, head)
  val tailTracks = (1 to 9).foldLeft(headTracks) { (accum, knotNo) =>
    calculateTailTracks(accum, tail)
  }
  tailTracks.toSet.size

def inputFileLoader(filename: String): List[Direction] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .map(Direction.from)
    .toList
