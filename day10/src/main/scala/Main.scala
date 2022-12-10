package day10

import scala.io.Source

@main def main: Unit =
  val sampleCase = inputFileLoader("/input_sample")
  val testCase   = inputFileLoader("/input")

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

def part1(instructions: List[Input]): Int =
  val registerRecording = instructions
    .foldLeft(List(Register(0, 1)))((accum, instruction) =>
      val currentState = accum.takeRight(1)(0)
      val newState = instruction match
        case Input.Noop    => List(computeNoop(currentState))
        case Input.Addx(x) => computeAddx(currentState, x)
      accum ::: newState
    )

  (20 to registerRecording.length by 40)
    .map(i =>
      registerRecording(i).cycle * registerRecording(i - 1).value
    ) // the value of the register are the in the previous index
    .sum

// I'm so done
// https://www.reddit.com/r/adventofcode/comments/zhjfo4/comment/izmuj99/
def part2(instructions: List[Input]): String =
  val registerRecording = instructions
    .foldLeft(List(Register(0, 1)))((accum, instruction) =>
      val currentState = accum.takeRight(1)(0)
      val newState = instruction match
        case Input.Noop    => List(computeNoop(currentState))
        case Input.Addx(x) => computeAddx(currentState, x)
      accum ::: newState
    )

  registerRecording
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
