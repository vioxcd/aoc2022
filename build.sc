import mill._
import mill.scalalib._

object day1 extends SbtModule {
  def scalaVersion = "3.1.1"
  // https://com-lihaoyi.github.io/mill/mill/Configuring_Mill.html#_custom_test_frameworks
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}
object day2 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day3 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day4 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day5 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day6 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day7 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day8 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}

object day9 extends SbtModule {
  def scalaVersion = "3.1.1"
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}
