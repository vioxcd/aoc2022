val input = List(
  "30373",
  "25512",
  "65332",
  "33549",
  "35390"
)

val testCase = input.map(_.map(_.asDigit).toVector).toVector

val colLength      = testCase(0).length
val rowLength      = testCase.length
val edgeItemsCount = ((colLength - 2) * 2) + (rowLength * 2)

/** PART 1
  */
val res = for
  i <- (1 until colLength - 1)
  j <- (1 until rowLength - 1)
yield
  val column  = testCase.map(_(j))
  val row     = testCase(i)
  val current = testCase(i)(j)

  // LTR
  val fromLeft           = row.slice(0, j)
  val notVisibleFromLeft = fromLeft.exists(_ >= current)

  // RTL
  val fromRight           = row.slice(j + 1, colLength)
  val notVisibleFromRight = fromRight.exists(_ >= current)

  // Top-down
  val fromTop           = column.slice(0, i)
  val notVisibleFromTop = fromTop.exists(_ >= current)

  // Bottom-up
  val fromBottom           = column.slice(i + 1, rowLength)
  val notVisibleFromBottom = fromBottom.exists(_ >= current)

  notVisibleFromLeft && notVisibleFromRight && notVisibleFromTop && notVisibleFromBottom

res.groupBy(identity).mapValues(_.size)(false) + edgeItemsCount
