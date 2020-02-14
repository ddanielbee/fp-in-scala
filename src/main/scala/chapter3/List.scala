package chapter3

sealed trait List2[+A]
case object Nil extends List2[Nothing]
case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {

  def tail[A](xs: List2[A]): List2[A] = xs match {
    case Nil           => throw new Exception("Blow it up")
    case Cons(_, rest) => rest
  }

  def safeTail[A](xs: List2[A]): Option[List2[A]] = xs match {
    case Nil           => None
    case Cons(_, rest) => Some(rest)
  }

  def setHead[A](xs: List2[A], a: A): List2[A] = xs match {
    case Nil           => List2(a)
    case Cons(_, rest) => Cons(a, rest)
  }

  @scala.annotation.tailrec
  def drop[A](xs: List2[A], n: Int): List2[A] = xs match {
    case Nil           => Nil
    case Cons(_, rest) => if (n == 1) rest else drop(rest, n - 1)
  }

  def dropWhile[A](xs: List2[A], p: (A) => Boolean): List2[A] = xs match {
    case Nil           => Nil
    case Cons(x, rest) => if (p(x)) dropWhile(rest, p) else xs
  }

  def init[A](xs: List2[A]): List2[A] = xs match {
    case Nil           => throw new Exception("Blow it up")
    case Cons(_, Nil)  => Nil
    case Cons(h, rest) => Cons(h, init(rest))
  }

  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
