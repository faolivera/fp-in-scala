package faolivera.fp.datastructures

/**
 * Created by folivera on 17/11/15.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Branch(left, right) =>
      case l: Leaf => acc + 1
    }
    go(tree, 0)
  }
}


