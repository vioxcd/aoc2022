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

/** PART 2
  */
val res = for
  i <- (1 until colLength - 1)
  j <- (1 until rowLength - 1)
yield
  val column  = testCase.map(_(j))
  val row     = testCase(i)
  val current = testCase(i)(j)

  val fromLeft   = row.slice(0, j).reverse    // from tree to left
  val fromRight  = row.slice(j + 1, colLength)
  val fromTop    = column.slice(0, i).reverse // from tree to top
  val fromBottom = column.slice(i + 1, rowLength)

  val leftTreesUntilBlocked   = fromLeft.takeWhile(_ < current)
  val rightTreesUntilBlocked  = fromRight.takeWhile(_ < current)
  val topTreesUntilBlocked    = fromTop.takeWhile(_ < current)
  val bottomTreesUntilBlocked = fromBottom.takeWhile(_ < current)

  def checkBounds(computed: Vector[Int], source: Vector[Int]): Int =
    // if length is the same as computed, don't add 1 (don't count last "blocker" tree)
    if computed.length == source.length then computed.length else computed.length + 1

  val leftScenicScore   = checkBounds(leftTreesUntilBlocked, fromLeft)
  val rightScenicScore  = checkBounds(rightTreesUntilBlocked, fromRight)
  val topScenicScore    = checkBounds(topTreesUntilBlocked, fromTop)
  val bottomScenicScore = checkBounds(bottomTreesUntilBlocked, fromBottom)

  leftScenicScore * rightScenicScore * topScenicScore * bottomScenicScore

res.max
