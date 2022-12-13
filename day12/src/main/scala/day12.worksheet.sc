import scala.io.Source
import scala.collection.immutable.Queue

type Grid = Vector[Vector[Char]]
case class Coordinate(x: Int, y: Int)
case class Step(c: Coordinate, count: Int)
case class State(q: List[Coordinate], v: Set[Coordinate])

val fileName = "/home/uchan/code/fun/aoc2022/day12/src/main/resources/input_sample"
val _input: Grid = Source
  .fromFile(fileName)
  .getLines
  .map(_.split("").map(_.charAt(0)).toVector)
  .toVector

val start = _input.zipWithIndex.flatMap { case (e, i) =>
  if e.indexOf('S') != -1 then Some(Coordinate(e.indexOf('S'), i)) else None
}.head
val input     = _input.updated(start.y, _input(start.y).updated(start.x, 'a'))
val colLength = input(0).length
val rowLength = input.length
val visited   = Set(start)
val queue     = List(start)
val s         = State(queue, visited)

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
def checkBounds = bounds(rowLength, colLength)

def finish      = (grid: Grid) => (coord: Coordinate) => if grid(coord.y)(coord.x) == 'E' then true else false
def checkFinish = finish(input)

def move = (grid: Grid) =>
  (current: Coordinate, target: Coordinate) =>
    if (grid(target.y)(target.x) - grid(current.y)(current.x)) <= 1 then Some(target) else None
def checkMove = move(input)

// https://stackoverflow.com/a/8716751
def traverse(g: Grid, q: Seq[Step], v: Set[Coordinate]): Seq[Step] =
  if q.isEmpty then Seq.empty
  else
    val next = q.head
    val possiblePaths = generatePathways(next.c)
      .map(checkBounds)
      .flatten
      .map(checkMove(next.c, _))
      .flatten
      .map(Step(_, next.count + 1))
      .toSet

    val succ = possiblePaths.filterNot((step: Step) => v(step.c)).filterNot(q.contains(_))
    next +: traverse(g, q.tail ++ succ, v + next.c)

def traverseFrom(g: Grid, initial: Step) =
  traverse(g, Seq(initial), Set.empty)

val res = traverseFrom(input, Step(start, 0))
res.takeRight(1).head.count + 1

// def breadth_first_traverse(node: Coordinate, f: Coordinate => Queue[Coordinate]): LazyList[Coordinate] = {
//   def recurse(q: Queue[Coordinate]): LazyList[Coordinate] = {
//     if (q.isEmpty) {
//       LazyList.empty
//     } else {
//       val (node, tail) = q.dequeue
//       node #:: recurse(tail ++ f(node))
//     }
//   }
//   node #:: recurse(Queue.empty ++ f(node))
// }

// breadth_first_traverse(start, f)

// val f = (node: Coordinate) =>
//   Queue.empty ++ generatePathways(node)
//     .map(checkBounds)
//     .flatten
//     .map(checkMove(node, _))
//     .flatten
