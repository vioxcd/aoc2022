import mill._
import mill.scalalib._

object aoc2022 extends SbtModule {
  def scalaVersion = "3.1.1"
  // https://com-lihaoyi.github.io/mill/mill/Configuring_Mill.html#_custom_test_frameworks
  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.9")
  }
}
