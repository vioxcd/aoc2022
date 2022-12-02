package day2

import scala.io.Source

object Main {
  enum RPS:
    case Rock
    case Paper
    case Scissors

  enum WDL:
    case Win
    case Draw
    case Lose

  val Score: Map[RPS | WDL, Int] = Map(
    RPS.Rock -> 1,
    RPS.Paper -> 2,
    RPS.Scissors -> 3,
    WDL.Lose -> 0,
    WDL.Draw -> 3,
    WDL.Win -> 6
  )

  type RPSPair = (RPS, RPS)
  type RPSWDLPair = (RPS, WDL)

  object SecondColumn:
    def parseAsRPS(s: String): RPSPair =
      s match
        case s"$x $y" => (rps(x), rps(y))
        case _        => throw new Exception(s"value $s is not valid command")

    def parseAsWDL(s: String): RPSWDLPair =
      s match
        case s"$x $y" => (rps(x), wdl(y))
        case _        => throw new Exception(s"value $s is not valid command")

    def rps(s: String): RPS =
      s match
        case "A" | "X" => RPS.Rock
        case "B" | "Y" => RPS.Paper
        case "C" | "Z" => RPS.Scissors

    def wdl(s: String): WDL =
      s match
        case "X" => WDL.Lose
        case "Y" => WDL.Draw
        case "Z" => WDL.Win

  object Game:
    def rps(opp: RPS, me: RPS): WDL =
      (me, opp) match
        case (RPS.Rock, RPS.Paper)     => WDL.Lose
        case (RPS.Rock, RPS.Scissors)  => WDL.Win
        case (RPS.Paper, RPS.Scissors) => WDL.Lose
        case (RPS.Paper, RPS.Rock)     => WDL.Win
        case (RPS.Scissors, RPS.Rock)  => WDL.Lose
        case (RPS.Scissors, RPS.Paper) => WDL.Win
        case _                         => WDL.Draw

    def wdl(opp: RPS, me: WDL): RPS =
      (me, opp) match
        case (WDL.Win, RPS.Paper)     => RPS.Scissors
        case (WDL.Lose, RPS.Paper)    => RPS.Rock
        case (WDL.Win, RPS.Rock)      => RPS.Paper
        case (WDL.Lose, RPS.Rock)     => RPS.Scissors
        case (WDL.Win, RPS.Scissors)  => RPS.Rock
        case (WDL.Lose, RPS.Scissors) => RPS.Paper
        case (WDL.Draw, _)            => opp

  def main(args: Array[String]): Unit =
    val testCase =
      Source
        .fromFile(getClass.getResource("/input").getFile)
        .getLines
        .toSeq

    println(s"--- First input ---")
    println(part1(testCase))

    println(s"--- First input ---")
    println(part2(testCase))

  def part1(matches: Seq[String]): Int =
    matches
      .map(s => SecondColumn.parseAsRPS(s))
      .map((opp, me) => (Game.rps(opp, me), me))
      .map((matchResult, myMove) => Score(matchResult) + Score(myMove))
      .sum

  def part2(matches: Seq[String]): Int =
    matches
      .map(s => SecondColumn.parseAsWDL(s))
      .map((opp, me) => (Game.wdl(opp, me), me))
      .map((myMove, matchResult) => Score(matchResult) + Score(myMove))
      .sum
}
