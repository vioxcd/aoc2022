import scala.io.Source

enum Input:
  case Noop
  case Addx(x: Int)

object Input:
  def from(s: String): Input = s match
    case "noop"     => Input.Noop
    case s"addx $x" => Input.Addx(x.toInt)
    case _          => throw new Exception(s"wrong input detected. got $s")

case class Register(cycle: Int, value: Int)

val fileName = "/home/uchan/code/fun/aoc2022/day10/src/main/resources/input_sample"
val input = Source
  .fromFile(fileName)
  .getLines
  .toList
val testCase = input.map(Input.from)

val compute     = (currentState: Register, x: Int) => Register(currentState.cycle + 1, currentState.value + x)
val computeNoop = (currentState: Register) => compute(currentState, 0)
val computeAddx = (currentState: Register, x: Int) =>
  val res1 = compute(currentState, 0)
  val res2 = compute(res1, x)
  List(res1, res2)

val registerRecording = testCase
  .foldLeft(List(Register(0, 1)))((accum, instruction) =>
    val currentState = accum.takeRight(1)(0)
    val newState = instruction match
      case Input.Noop    => List(computeNoop(currentState))
      case Input.Addx(x) => computeAddx(currentState, x)
    accum ::: newState
  )
// .drop(1)

val x = registerRecording.slice(218, 223)
registerRecording.length
// (20 to registerRecording.length by 40).toList

(20 to registerRecording.length by 40)
  .map(i => registerRecording(i).cycle * registerRecording(i - 1).value)
  .sum
