import scala.io.Source

type Grid = Vector[Vector[Char]]
case class Coordinate(x: Int, y: Int)
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

val input = _input.updated(start.y, _input(start.y).updated(start.x, 'a'))

val colLength = input(0).length
val rowLength = input.length
val visited   = Set(start)
val queue     = List(start)
val s         = State(queue, visited)

val res = Iterator
  .unfold((0, s)) { (steps, state) =>
    println(s"Steps: $steps")

    if !state.q.nonEmpty then None

    val current :: rest = state.q
    val v               = state.v + current
    val x = generatePathways(current)
      .map(checkBounds)
      .flatten
      .filterNot(v(_))
      .map(checkMove(current, _))
      .flatten

    val y = x.map(checkFinish).exists(_ == true)

    if y then
      println("Finished")
      None
    else
      val q = rest ::: x
      Some((steps, (steps + 1, State(q, v))))
  }
  .toList

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

// val x = generatePathways(start)
//   .map(checkBounds)
//   .flatten
//   .filterNot(visited(_))

// val y = x.map(checkFinish).exists(_ == true)

// if y then println("Finished")
// else for c <- x yield explore(c)

/*
def explore(
    start: Coordinate,
    visited: Set[Coordinate],
    stepCount: Int,
    steps: List[Int],
    grid: Grid = input
): List[Int] =
  println(s"Exploring: $start")
  println(s"Step: $steps")
  println(s"Step Count: $stepCount")

  val v = visited + start

  val x = generatePathways(start)
    .map(checkBounds)
    .flatten
    .filterNot(visited(_))
    .map(checkMove(start, _))
    .flatten

  val y = x.map(checkFinish).exists(_ == true)

  if y then
    println("Finished")
    steps :+ stepCount + 1
  else
    x.foldLeft(steps) { (accum, c) => if accum.nonEmpty then accum else explore(c, v, stepCount + 1, accum) }
    // val ret = for c <- x yield explore(c, v, stepCount + 1, steps)
    // ret.flatten

explore(start, visited, 0, List())
 */
