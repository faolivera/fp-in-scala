package faolivera.fp.gettingstarted

import scala.annotation.tailrec

/**
 * Created by folivera on 11/11/15.
 */
object GettingStarted {

  object LoopsFunctional {

    def fib(n: Int): Int = {
      @tailrec
      def loop(n: Int, prev: Int, cur: Int): Int =
        if (n == 0) prev
        else loop(n - 1, cur, prev + cur)
      loop(n, 0, 1)
    }
  }

  object PolymorphicFunctions {

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      @tailrec
      def go(idx: Int): Boolean = {
        idx >= as.length - 1 ||
          ( ordered(as(idx), as(idx + 1)) &&
            go(idx + 1) )
      }
      go(0)
    }
  }

  object FollowingTypes {

    def curry[A,B,C](f: (A, B) => C): A => (B => C) =
      a => b => f(a, b)

    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
      (a, b) => f(a)(b)

    def compose[A,B,C](f: B => C, g: A => B): A => C =
      a => f(g(a))
  }

}

