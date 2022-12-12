import scala.io.Source

// import scala.annotation.tailrec
// @tailrec
// final def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
// def lcm(a: Int, b: Int): Int       = a / gcd(a, b) * b

case class Monkey(
    no: Int,
    items: List[Int],
    newWorriedLevel: Int => Int,
    nextMonkeyPassed: Int => Int,
    divisibility: Int
)
case class PassedItem(from: Int, to: Int, worriedLevel: Int)
case class RoundCounter(monkeys: Vector[Monkey], inspectionCount: Map[Int, Int])

val monkeys = Vector(
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

// val lcmDivs = monkeys.map(_.divisibility).reduce(lcm)
// lcmDivs

val inspectionCounter = (0 to 3).map((_, 0)).toMap
val roundCounter      = RoundCounter(monkeys, inspectionCounter)

// val x = (1 to 20).foldLeft(roundCounter) { (counter, round) =>
val x = (1 until 2).foldLeft(roundCounter) { (counter, round) =>
// println(s"Round $round")
  counter.monkeys.foldLeft(counter) { (accum, monkey) =>
    println(s"Monkey ${monkey.no}")
    // if monkey.items.length != 0 then
    val currentItems = accum.monkeys(monkey.no).items
    if currentItems.length != 0 then
      val passedItems = for item <- currentItems yield
        println(s"Monkey inspects an item with a worry level of $item.")

        val worryLevel = monkey.newWorriedLevel(item)
        println(s"Worry level is multiplied to $worryLevel.")

        val reducedWorryLevel = worryLevel / 3
        println(s"Monkey gets bored with item. Worry level is divided by 3 to $reducedWorryLevel.")

        val passedTo = monkey.nextMonkeyPassed(reducedWorryLevel)
        println(s"Current worry level is not divisible by ${monkey.divisibility}.")
        println(s"Item with worry level $reducedWorryLevel is thrown to monkey $passedTo.")
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
      print(currentInspectionCounter)
      RoundCounter(currentMonkeys, currentInspectionCounter)
    else
      println(s"No items for Monkey ${monkey.no}")
      accum
  }
}

x.inspectionCount.values.toList.sorted.takeRight(2).reduce(_ * _)
