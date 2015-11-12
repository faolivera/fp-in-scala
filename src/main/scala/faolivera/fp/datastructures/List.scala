package faolivera.fp.datastructures

import java.util.NoSuchElementException
import annotation.tailrec
/**
 * Created by folivera on 12/11/15.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](l: List[A]): A = l match {
    case Nil => throw NoSuchElementException
    case Cons(x, _) => x
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => throw NoSuchElementException
    case (_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("Nil init")
    case (_, Nil) => Nil
    case (x, xs) => Cons(x, init(xs))
  }
}
