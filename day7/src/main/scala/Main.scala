package day7

import scala.io.Source

@main def main: Unit =
  val testCase =
    Source
      .fromFile(getClass.getResource("/input").getFile)
      .getLines
      .map(Terminal.from)
      .toList

  println(s"--- First input ---")
  println(part1(testCase))

  println(s"--- Second input ---")
  println(part2(testCase))

enum Terminal:
  case GoToDirectory(path: String)
  case BackUpDirectory
  case ListDirectory
  case Directory(name: String)
  case File(name: String, size: Int)

object Terminal:
  def from(s: String): Terminal =
    s match
      case s"$$ cd .."    => BackUpDirectory
      case s"$$ cd $path" => GoToDirectory(path)
      case s"$$ ls"       => ListDirectory
      case s"dir $name"   => Directory(name)
      case s"$size $name" => File(name, size.toInt)
      case _              => throw new Exception(s"value $s is not valid command")

def mapDirectoryNameToSize(stack: List[String], size: Int): List[(String, Int)] =
  // directory name are stored in the stack
  stack
    .slice(1, stack.length) // skip "/" path, so that the name looks somewhat ok
    .scanLeft("/")(_ + _ + "/")
    .map((_, size))

def calculateDirSizes(lines: List[Terminal]): Map[String, Int] =
  // ignore the used stack, only calculated sizes is used
  val (_, sizes) =
    lines.foldLeft[(List[String], List[(String, Int)])]((List.empty, List.empty)) {
      (acc, terminal) =>
        val (stack, dirSizes) = acc

        terminal.match
          case Terminal.BackUpDirectory     => (stack.dropRight(1), dirSizes) // pop
          case Terminal.GoToDirectory(path) => (stack :+ path, dirSizes)      // push
          case Terminal.File(name, size) => (stack, dirSizes ++ mapDirectoryNameToSize(stack, size))
          case _                         => (stack, dirSizes)
    }
  // map all (directory, size) to directory -> sum(sizes)
  sizes.groupMap(_._1)(_._2).mapValues(_.sum).toMap

def part1(lines: List[Terminal]): Int =
  val limit    = 100000
  val dirSizes = calculateDirSizes(lines)
  dirSizes.values.filter(_ <= limit).sum

def part2(lines: List[Terminal]): Int =
  val dirSizes    = calculateDirSizes(lines)
  val diskSpace   = 70000000
  val freeSpace   = diskSpace - dirSizes("/")
  val neededSpace = 30000000 - freeSpace
  dirSizes.values.toList.sorted.dropWhile(_ < neededSpace).head
