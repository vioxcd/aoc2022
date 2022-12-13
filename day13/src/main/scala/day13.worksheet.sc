import scala.io.Source

case class Stepper(s: String, level: Int = 0, index: Int = 0)

val fileName = "/home/uchan/code/fun/aoc2022/day13/src/main/resources/input_extra"
// val fileName = "/home/uchan/code/fun/aoc2022/day13/src/main/resources/input_sample"
val input = Source
  .fromFile(fileName)
  .getLines
  .toList

val testCase = input
  .filterNot(_ == "")
  .grouped(2)
  .toList

// val x :: y :: _ = testCase(0)
// val s1          = Stepper(x.replace(",", ""), 0)
// val s2          = Stepper(x.replace(",", ""), 0)

// def recur(s1: Stepper, s2: Stepper): Boolean =
//   if s1.index == s1.s.length || s2.index == s2.s.length then true else false

// recur(s1, s2)
def checkLift(s: String, i: Int): Boolean =
  println(s"Checking lift for $s $i")
  if s(i + 1) == ']' then true else false

def compare(pair: List[String], index: (Int, Int) = (-1, -1), level: (Int, Int) = (0, 0)): Boolean =
  val s1 :: s2 :: _ = pair
  val (li, ri)      = index

  if li == s1.length then
    println("Left side ran out of items, so inputs are in the right order")
    true
  else if ri == s2.length then
    println("Right side ran out of items, so inputs are not in the right order")
    false
  else if li == -1 && ri == -1 then
    // start
    println(s"Compare $s1 vs $s2")
    val startIndex = (0, 0)
    compare(pair, startIndex)
  else
    // recur
    val i1 = s1(li)
    val i2 = s2(ri)
    println(
      s"Compare $i1 vs $i2 on level $level"
    )

    val (isOkay, newLevel, newIndex) = (i1, i2) match
      case ('[', '[') => (true, (level._1 + 1, level._2 + 1), (li + 1, ri + 1))
      case ('[', ']') => (true, (level._1 + 1, level._2 - 1), (li + 1, ri + 1))
      case (']', '[') => (true, (level._1 - 1, level._2 + 1), (li + 1, ri + 1))
      case (']', ']') => (true, (level._1 - 1, level._2 - 1), (li + 1, ri + 1))
      case ('[', _) =>
        if checkLift(s2, ri) then (true, (level._1 + 1, level._2), (li + 1, ri)) else (false, level, index)
      case (_, '[') =>
        if checkLift(s1, li) then (true, (level._1, level._2 + 1), (li, ri + 1)) else (false, level, index)
      case (_, _) => (true, level, (li + 1, ri + 1))

    println(s"Status: isOkay: $isOkay - leftside: $i1 - rightside: $i2")
    if !isOkay then
      println("Failed to lift exactly one value integer")
      false
    else if isOkay && i1.isDigit && i2.isDigit && i1 < i2 then
      println("Left side is smaller, so inputs are in the right order")
      true
    else if isOkay && i1.isDigit && i2.isDigit && i1 > i2 then
      println("Right side is smaller, so inputs are not in the right order")
      false
    else compare(pair, newIndex, newLevel)

val res = testCase.foldLeft((0, 1)) { (t, pair) =>
  val (correctPairCount, pairNo) = t
  // correctPairCount + checkCorrectness(pair)
  println(s"== Pair $pairNo")
  if compare(pair.map(_.replace(",", "")).toList) then (correctPairCount + pairNo, pairNo + 1)
  else (correctPairCount, pairNo + 1)
}

res._1

// val createStepper = (s: String) => Stepper(s, 0)

// def checkCorrectness(pair: List[String]): Int =
//   val x :: y :: _ = pair
//   val s1          = x.replace(",", "").drop(1).dropRight(1) andThen createStepper
//   val s2          = y.replace(",", "").drop(1).dropRight(1) andThen createStepper

//   def recur(s1: Stepper, s2: Stepper): Boolean =
//     if s1.index == s1.s.length || s2.index == s2.s.length then
//       true
//     else
//       if
