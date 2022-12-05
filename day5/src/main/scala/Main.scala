package day5

import scala.io.Source

/** Answer
  *
  * --- First input --- TQRFCBSJJ
  * --- Second input --- RMHFJNVFP
  */

/** Sample Crates */
// val crates: List[Crate] = List(
//   List('Z', 'N'),
//   List('M', 'C', 'D'),
//   List('P')
// )

type Crate   = List[Char]
type Command = List[Int]

@main def main: Unit =
  val testCase: List[Command] =
    Source
      .fromFile(getClass.getResource("/input").getFile)
      .getLines
      .map(s => """\d+""".r findAllIn s)
      .map(_.map(_.toInt).toList)
      .toList

  /** Actual Crates */
  val crates: List[Crate] = List(
    List('M', 'J', 'C', 'B', 'F', 'R', 'L', 'H'),
    List('Z', 'C', 'D'),
    List('H', 'J', 'F', 'C', 'N', 'G', 'W'),
    List('P', 'J', 'D', 'M', 'T', 'S', 'B'),
    List('N', 'C', 'D', 'R', 'J'),
    List('W', 'L', 'D', 'Q', 'P', 'J', 'G', 'Z'),
    List('P', 'Z', 'T', 'F', 'R', 'H'),
    List('L', 'V', 'M', 'G'),
    List('L', 'V', 'M', 'G', 'C', 'B', 'G', 'P', 'F', 'Q', 'R', 'J')
  )

  println(s"--- First input ---")
  println(part1(crates, testCase))

  println(s"--- Second input ---")
  println(part2(crates, testCase))

def mover(crates: List[Crate], commands: List[Command], crateMover: Crate => Crate): String =
  val fc = commands.foldLeft(crates) { (c, command) =>
    val amount             = command(0)
    val from               = command(1) - 1
    val to                 = command(2) - 1
    val currentCrateLength = c(from).length

    if currentCrateLength > amount then
      val (stayed, moved) = c(from).splitAt(currentCrateLength - amount)
      c
        .updated(from, stayed)
        .updated(to, c(to) ++ crateMover(moved))
    else
      c
        .updated(from, List())
        .updated(to, c(to) ++ crateMover(c(from)))
  }
  fc.map(_.takeRight(1).head).mkString

def part1(crates: List[Crate], commands: List[Command]): String =
  def crate9000(l: Crate): Crate =
    l.reverse
  mover(crates, commands, crate9000)

def part2(crates: List[Crate], commands: List[Command]): String =
  def crate9001(l: Crate): Crate =
    l
  mover(crates, commands, crate9001)
