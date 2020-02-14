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

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit =
    println(fib(5))
}
