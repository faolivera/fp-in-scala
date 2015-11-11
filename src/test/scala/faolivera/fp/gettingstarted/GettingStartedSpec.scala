package faolivera.fp.gettingstarted

import org.specs2.mutable.Specification

/**
 * Created by folivera on 11/11/15.
 */
class GettingStartedSpec extends Specification {
  import GettingStarted._
  import LoopsFunctional._
  import PolymorphicFunctions._
  import FollowingTypes._

  "fib" should {
    "return 0 for n=0" in {
      fib(0) === 0
    }

    "return 1 for n=1" in {
      fib(1) === 1
    }

    "return 5 for n=5" in {
      fib(5) === 5
    }

    "return 8 for n=8" in {
      fib(5) === 5
    }
  }

  "isSorted" should {
    val sortedInts = Array(1,2,3,4)
    val unsortedInts = Array(1,3,2,4)
    val strings = Array("z", "db", "acf")

    def arrayToString(a: Array[_]): String = s"Array(${a.mkString(", ")})"

    s"return true for ${arrayToString(sortedInts)}" in {
      isSorted[Int](sortedInts, _ < _) === true
    }

    s"return false for ${arrayToString(unsortedInts)}" in {
      isSorted[Int](unsortedInts, _ < _) === false
    }

    s"sorted true by lenght for ${arrayToString(strings)}" in {
      isSorted[String](strings, _.length < _.length) === true
    }

    s"sorted false by first char for ${arrayToString(strings)}" in {
      isSorted[String](strings, _.head < _.head) === false
    }
  }

  val sum = (a: Int, b: Int) => a + b
  val curryiedSum = (a: Int) => (b: Int) => a + b

  "curry" should {
    "curry sum correctly" in {
      curry(sum)(1)(2) === sum(1, 2)
      curry(sum)(4)(5) === sum(4, 5)
    }
  }

  "uncurry" should {
    "uncurry curryiedSum correctly" in {
      uncurry(curryiedSum)(1, 2) === curryiedSum(1)(2)
      uncurry(curryiedSum)(4, 5) === curryiedSum(4)(5)
    }
  }

}
