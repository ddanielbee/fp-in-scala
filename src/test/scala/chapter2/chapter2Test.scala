package chapter2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class chapter2Test extends AnyFlatSpec with Matchers {
  behavior of "chapter 2"

  behavior of "fib"

  it should "calculate the correct Fibonacci number" in {
    chapter2.fib(5) shouldBe 8
  }

  it should "return 0 for a negative number" in {
    chapter2.fib(-1) shouldBe 0
  }

  behavior of "isSorted"

  it should "return false for an unsorted INT array with a descending order sorting fn" in {
    chapter2.isSorted(List(1, 2, 3), (a: Int, b: Int) => a > b) shouldBe false
  }

  it should "return true for a sorted INT list with an ascending order sorting fn" in {
    chapter2.isSorted(List(1, 2, 3), (a: Int, b: Int) => a < b) shouldBe true
  }

}
