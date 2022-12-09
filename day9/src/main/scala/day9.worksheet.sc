case class Direction(where: String, distance: Int)
case class Coordinate(x: Int, y: Int)

val testInput = List(
  Direction("R", 4),
  Direction("U", 4),
  Direction("L", 3),
  Direction("D", 1),
  Direction("R", 4),
  Direction("D", 1),
  Direction("L", 5),
  Direction("R", 2)
)

def calculatePositionDiff(head: Coordinate, tail: Coordinate): Coordinate =
  Coordinate(head.x - tail.x, head.y - tail.y)

def moveTail(tail: Coordinate, diff: Coordinate): Coordinate =
  Coordinate(tail.x + diff.x.sign, tail.y + diff.y.sign)

def calculateTailPosition(head: Coordinate, tail: Coordinate): Coordinate =
  val diff    = calculatePositionDiff(head, tail)
  val absDiff = Coordinate(diff.x.abs, diff.y.abs)

  absDiff match
    case _ if absDiff.x == absDiff.y     => tail                 // same position
    case _ if absDiff.x + absDiff.y == 1 => tail                 // not behind
    case _ if absDiff.x + absDiff.y > 1  => moveTail(tail, diff) // behind

  // Coordinate(diff.x.abs, diff.y.abs) match
  //   case Coordinate(0, 0) | Coordinate(1, 1) | Coordinate(1, 0) | Coordinate(0, 1) => tail // not behind
  //   case Coordinate(2, 0) | Coordinate(0, 2) | Coordinate(2, 1) | Coordinate(1, 2) => moveTail(tail, diff)
  //   case Coordinate(_, _) => throw new Exception(s"position calculation went wrong: got $diff")

val headCoord = Coordinate(0, 0)
val tailCoord = Coordinate(0, 0)

val headTracks = testInput.foldLeft(List(headCoord)) { (accum, direction) =>
  val current = accum.takeRight(1)(0)
  val tracks = direction.where match
    case "R" => (1 to direction.distance).map(step => Coordinate(current.x + step, current.y)).toList
    case "L" => (1 to direction.distance).map(step => Coordinate(current.x - step, current.y)).toList
    case "U" => (1 to direction.distance).map(step => Coordinate(current.x, current.y + step)).toList
    case "D" => (1 to direction.distance).map(step => Coordinate(current.x, current.y - step)).toList
    case _   => throw new Exception(s"value $direction is not a valid direction")
  accum ::: tracks
}

val tailTracks = headTracks.foldLeft(List(tailCoord)) { (accum, headTrack) =>
  val current = accum.takeRight(1)(0)
  val tail    = calculateTailPosition(headTrack, current)
  accum :+ tail
}

tailTracks.toSet
tailTracks.toSet.size
