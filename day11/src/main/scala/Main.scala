package day11

import scala.io.Source

@main def main: Unit =
  val sampleMonkeys = Vector(
    Monkey(
      0,
      List(79, 98),
      old => old * 19,
      worriedLevel => if worriedLevel % 23 == 0 then 2 else 3,
      23
    ),
    Monkey(
      1,
      List(54, 65, 75, 74),
      old => old + 6,
      worriedLevel => if worriedLevel % 19 == 0 then 2 else 0,
      19
    ),
    Monkey(
      2,
      List(79, 60, 97),
      old => old * old,
      worriedLevel => if worriedLevel % 13 == 0 then 1 else 3,
      13
    ),
    Monkey(
      3,
      List(74),
      old => old + 3,
      worriedLevel => if worriedLevel % 17 == 0 then 0 else 1,
      17
    )
  )

  val testMonkeys = Vector(
    Monkey(
      0,
      List(54, 82, 90, 88, 86, 54),
      old => old * 7,
      worriedLevel => if worriedLevel % 11 == 0 then 2 else 6,
      11
    ),
    Monkey(
      1,
      List(91, 65),
      old => old * 13,
      worriedLevel => if worriedLevel % 5 == 0 then 7 else 4,
      5
    ),
    Monkey(
      2,
      List(62, 54, 57, 92, 83, 63, 63),
      old => old + 1,
      worriedLevel => if worriedLevel % 7 == 0 then 1 else 7,
      7
    ),
    Monkey(
      3,
      List(67, 72, 68),
      old => old * old,
      worriedLevel => if worriedLevel % 2 == 0 then 0 else 6,
      2
    ),
    Monkey(
      4,
      List(68, 89, 90, 86, 84, 57, 72, 84),
      old => old + 7,
      worriedLevel => if worriedLevel % 17 == 0 then 3 else 5,
      17
    ),
    Monkey(
      5,
      List(79, 83, 64, 58),
      old => old + 6,
      worriedLevel => if worriedLevel % 13 == 0 then 3 else 0,
      13
    ),
    Monkey(
      6,
      List(96, 72, 89, 70, 88),
      old => old + 4,
      worriedLevel => if worriedLevel % 3 == 0 then 1 else 2,
      3
    ),
    Monkey(
      7,
      List(79),
      old => old + 8,
      worriedLevel => if worriedLevel % 19 == 0 then 4 else 5,
      19
    )
  )

  // val pt1Sample = part1(sampleMonkeys)
  // val pt1Test   = part1(testMonkeys)

  // println("--- First case ---")
  // println(s"sample: $pt1Sample")
  // println(s"actual: $pt1Test")

  val pt2Sample = part2(sampleMonkeys)
  // val pt2Test   = part2(testMonkeys)

  println("\n--- Second input ---")
  println(s"sample: $pt2Sample")
  // println(s"actual: $pt2Test")

case class Monkey(
    no: Int,
    items: List[BigInt],
    newWorriedLevel: BigInt => BigInt,
    nextMonkeyPassed: BigInt => Int,
    divisibility: BigInt
)
case class PassedItem(from: Int, to: Int, worriedLevel: BigInt)
case class RoundCounter(monkeys: Vector[Monkey], inspectionCount: Map[Int, BigInt])

def part1(monkeys: Vector[Monkey]): BigInt =
  val inspectionCounter: Map[Int, BigInt] = (0 until monkeys.length).map[(Int, BigInt)]((_, 0)).toMap
  val roundCounter                        = RoundCounter(monkeys, inspectionCounter)

  val x = (1 to 20).foldLeft(roundCounter) { (counter, round) =>
    // println(s"Round $round")
    counter.monkeys.foldLeft(counter) { (accum, monkey) =>
      // println(s"Monkey ${monkey.no}")
      // if monkey.items.length != 0 then
      val currentItems = accum.monkeys(monkey.no).items
      if currentItems.length != 0 then
        val passedItems = for item <- currentItems yield
          // println(s"Monkey inspects an item with a worry level of $item.")

          val worryLevel = monkey.newWorriedLevel(item)
          // println(s"Worry level is multiplied to $worryLevel.")

          val reducedWorryLevel = worryLevel / 3
          // println(s"Monkey gets bored with item. Worry level is divided by 3 to $reducedWorryLevel.")

          val passedTo = monkey.nextMonkeyPassed(reducedWorryLevel)
          // println(s"Current worry level is not divisible by ${monkey.divisibility}.")
          // println(s"Item with worry level $reducedWorryLevel is thrown to monkey $passedTo.")
          PassedItem(monkey.no, passedTo, reducedWorryLevel)

        val currentMonkeys = passedItems.foldLeft(accum.monkeys) { (acc, item) =>
          acc
            .updated(item.from, acc(item.from).copy(items = List()))
            .updated(item.to, acc(item.to).copy(items = acc(item.to).items :+ item.worriedLevel))
        }
        val currentInspectionCounter = passedItems.foldLeft(accum.inspectionCount) { (acc, item) =>
          val oldValue = acc(item.from)
          acc
            .updated(item.from, oldValue + 1)
        }
        RoundCounter(currentMonkeys, currentInspectionCounter)
      else
        // println(s"No items for Monkey ${monkey.no}")
        accum
    }
  }
  x.inspectionCount.values.toList.sorted.takeRight(2).reduce(_ * _)

def part2(monkeys: Vector[Monkey]): BigInt =
  val inspectionCounter: Map[Int, BigInt] = (0 until monkeys.length).map[(Int, BigInt)]((_, 0)).toMap
  val roundCounter                        = RoundCounter(monkeys, inspectionCounter)

  println(roundCounter)

  val x = (1 to 10000).foldLeft(roundCounter) { (counter, round) =>
    println(s"Processing Round $round")
    counter.monkeys.foldLeft(counter) { (accum, monkey) =>
      val currentItems = accum.monkeys(monkey.no).items
      if currentItems.length != 0 then
        val passedItems = for item <- currentItems yield
          val worryLevel        = monkey.newWorriedLevel(item)
          val reducedWorryLevel = worryLevel
          val passedTo          = monkey.nextMonkeyPassed(reducedWorryLevel)
          PassedItem(monkey.no, passedTo, reducedWorryLevel)
        val currentMonkeys = passedItems.foldLeft(accum.monkeys) { (acc, item) =>
          acc
            .updated(item.from, acc(item.from).copy(items = List()))
            .updated(item.to, acc(item.to).copy(items = acc(item.to).items :+ item.worriedLevel))
        }
        val currentInspectionCounter = passedItems.foldLeft(accum.inspectionCount) { (acc, item) =>
          val oldValue = acc(item.from)
          acc
            .updated(item.from, oldValue + 1)
        }
        RoundCounter(currentMonkeys, currentInspectionCounter)
      else accum
    }
  }
  x.inspectionCount.values.toList.sorted.takeRight(2).reduce(_ * _)

def inputFileLoader(filename: String): Iterator[String] =
  Source
    .fromFile(getClass.getResource(filename).getFile)
    .getLines
