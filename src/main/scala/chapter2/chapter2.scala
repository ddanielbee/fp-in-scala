package chapter2

object chapter2 {

  def fib(n: Int): Int =
    if (n < 0) {
      0
    } else if (n <= 1) {
      1
    } else {
      fib(n - 1) + fib(n - 2)
    }

  @scala.annotation.tailrec
  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    as match {
      case Nil      => true
      case _ :: Nil => true
      case h :: t   => if (ordered(h, t.head)) isSorted(t, ordered) else false
    }
  }

  def main(args: Array[String]): Unit =
    println(fib(5))
}
