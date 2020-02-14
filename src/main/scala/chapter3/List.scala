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

  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
