package day10

import scala.io.Source

@main def main: Unit =
  val testCase = inputFileLoader("/input")

  val registerState = computeRegisterState(testCase)

  val pt1Test = part1(registerState)
  val pt2Test = part2(registerState)

  println("--- First case ---")
  println(s"actual: $pt1Test")

  println("\n--- Second input ---")
  println(s"actual: $pt2Test")

enum Input:
  case Noop
  case Addx(x: Int)

object Input:
  def from(s: String): Input = s match
    case "noop"     => Input.Noop
    case s"addx $x" => Input.Addx(x.toInt)
    case _          => throw new Exception(s"wrong input detected. got $s")

case class Register(cycle: Int, value: Int)

val compute     = (currentState: Register, x: Int) => Register(currentState.cycle + 1, currentState.value + x)
val computeNoop = (currentState: Register) => compute(currentState, 0)
val computeAddx = (currentState: Register, x: Int) =>
  val res1 = computeNoop(currentState)
  val res2 = compute(res1, x)
  List(res1, res2)

def computeRegisterState(instructions: List[Input]): List[Register] =
  instructions
    .foldLeft(List(Register(0, 1)))((accum, instruction) =>
      val currentState = accum.takeRight(1)(0)
      val newState = instruction match
        case Input.Noop    => List(computeNoop(currentState))
        case Input.Addx(x) => computeAddx(currentState, x)
      accum ::: newState
    )

def part1(registerState: List[Register]): Int =
  (20 to registerState.length by 40)
    .map(i =>
      registerState(i).cycle * registerState(i - 1).value
    ) // the value of the register are the in the previous index
    .sum

// I'm so done
// https://www.reddit.com/r/adventofcode/comments/zhjfo4/comment/izmuj99/
def part2(registerState: List[Register]): String =
  registerState
    .foldLeft(new StringBuilder(240)) { case (sb, r) =>
      if r.value - 1 to r.value + 1 contains r.cycle % 40 then sb.append("#")
      else sb.append(".")
    }
    .toString()
    .grouped(40)
    .mkString("\n", "\n", "")

def inputFileLoader(filename: String): List[Input] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
    .map(Input.from)
    .toList

// Nice python solutions
// https://www.reddit.com/r/adventofcode/comments/zhjfo4/comment/izmtyb1/
// https://www.reddit.com/r/adventofcode/comments/zhjfo4/comment/izmtci5/
