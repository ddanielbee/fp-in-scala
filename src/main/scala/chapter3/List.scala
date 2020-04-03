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

  @scala.annotation.tailrec
  def dropWhile[A](xs: List2[A], p: (A) => Boolean): List2[A] = xs match {
    case Nil           => Nil
    case Cons(x, rest) => if (p(x)) dropWhile(rest, p) else xs
  }

  def append[A](a1: List2[A], a2: List2[A]): List2[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List2[A], a2: List2[A]): List2[A] =
    foldRight2(a1, a2)(Cons(_, _))

  def concat[A](ls: List2[List2[A]]): List2[A] =
    foldRight(ls, Nil: List2[A])(append)

  def foldRight[A, B](as: List2[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A, B](as: List2[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List2[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def length[A](as: List2[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  def sum(as: List2[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product(as: List2[Int]): Int =
    foldLeft(as, 1)(_ * _)

  def length2[A](as: List2[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List2[A]): List2[A] =
    foldLeft(as, List2[A]())((acc, cur) => Cons(cur, acc))

  def init[A](xs: List2[A]): List2[A] = xs match {
    case Nil           => throw new Exception("Blow it up")
    case Cons(_, Nil)  => Nil
    case Cons(h, rest) => Cons(h, init(rest))
  }

  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
