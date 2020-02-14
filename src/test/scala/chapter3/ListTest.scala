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

  behavior of "setHead"

  it should "add the item to the empty list" in {
    List2.setHead(Nil, 1) shouldBe List2(1)
  }

  it should "replace the first item of a non-empty list" in {
    List2.setHead(List2(1, 2), 3) shouldBe List2(3, 2)
  }

  behavior of "drop"

  it should "return an empty list when there's nothing else to drop" in {
    List2.drop(Nil, 50) shouldBe Nil
  }

  it should "remove the first n elements of the list" in {
    List2.drop(List2(1, 2, 3, 4, 5), 3) shouldBe List2(4, 5)
  }

  behavior of "dropWhile"

  it should "return an empty list when there's nothing else to drop" in {
    List2.dropWhile(Nil, (a: Int) => a > 3) shouldBe Nil
  }

  it should "remove elements as long as they fulfill the predicate" in {
    List2.dropWhile(List2(5, 6, 7, 1), (a: Int) => a > 3) shouldBe List2(1)
  }
}
