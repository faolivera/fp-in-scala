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
    case Nil => throw new NoSuchElementException
    case Cons(x, _) => x
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, xs) => Cons(h, xs)
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
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, => B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length(as: List[_]): Int = foldRight(as, 0)( (_, acc) => 1 + acc)

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def shortCircuitProduct(ns: List[Double]) = foldRight(ns, 1.0) { (e, acc) =>
    if(e == 0.0) 0.0
    else e * acc
  }

  def sum(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

  def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())( (acc, e) => Cons(e, acc) )

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z) ( (b, a) => f(a, b) )

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)( (a, b) => f(b, a) )

  def append[A](e: A, as: List[A]) = foldRight(as, List(e))( Cons(_, _) )
}
