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

  behavior of "curry"

  it should "have the exact same result for the curried & uncurried versions" in {
    def uncurried(a: Int, b: Int) = a * b
    val curried = chapter2.curry(uncurried)
    uncurried(2, 3) shouldBe curried(2)(3)
  }

  behavior of "uncurry"

  it should "have the exact same result for the curried & uncurried versions" in {
    def curried(a: Int) = (b: Int) => a * b
    val uncurried = chapter2.uncurry(curried)
    uncurried(2, 3) shouldBe curried(2)(3)
  }

  behavior of "compose"

  it should "have the same result for the composed version as calling the functions in order" in {
    def fn1(a: Int) = a * 2
    def fn2(c: Int) = c + 5
    chapter2.compose(fn1, fn2)(2) shouldBe fn1(fn2(2))
  }

}
