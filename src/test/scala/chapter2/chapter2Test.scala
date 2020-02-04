package chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class chapter2Test extends AnyFlatSpec with Matchers {
  behavior of "chapter 2"

  it should "calculate the correct Fibonacci number" in {
    chapter2.fib(5) shouldBe 8
  }

  it should "return 0 for a negative number" in {
    chapter2.fib(-1) shouldBe 0
  }

}
