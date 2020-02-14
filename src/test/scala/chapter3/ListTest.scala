package chapter3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpec with Matchers {
  behavior of "List2"

  behavior of "tail"

  it should "remove the first element of a List2" in {
    val initialList = List2(1, 2)
    List2.tail(initialList) shouldBe List2(2)
  }

  it should "blow up if the list is empty" in {
    assertThrows[Exception] {
      List2.tail(Nil)
    }
  }

  behavior of "safeTail"

  it should "remove the first element of a List2 and return Some of that list" in {
    val initialList = List2(1, 2)
    List2.safeTail(initialList) shouldBe Some(List2(2))
  }

  it should "return None when the list is empty" in {
    List2.safeTail(Nil) shouldBe None
  }
}
