package faolivera.fp.datastructures

import org.specs2.mutable.Specification
/**
 * Created by folivera on 12/11/15.
 */
class ListSpec extends Specification {
  import List._

  "append" should {
    "return a list with a new element in the last position" in {
      List(1, 2, 3, 4) === append(4, List(1, 2, 3))
    }
  }

  "flatList" should {
    "return List(List(1,2,3), List(4,5,6)) flatted" in {
      List(1, 2, 3, 4, 5, 6) === flatList(List(List(1,2,3), List(4,5,6)))
    }
  }

  "flatListRight" should {
    "return List(List(1,2,3), List(4,5,6)) flatted" in {
      List(1, 2, 3, 4, 5, 6) === flatListRight(List(List(1,2,3), List(4,5,6)))
    }
  }

  "sum1" should {
    "return List(2,3,4) from List(1,2,3)" in {
      List(2, 3, 4) === sum1(List(1,2,3))
    }
  }

  "doubleToString" should {
    "return List(\"1.0\", \"2.0\", \"3.0\") from List(1,2,3)" in {
      List("1.0", "2.0", "3.0") === doubleToString(List(1.0,2.0,3.0))
    }
  }

  "removeOddNumbers" should {
    "Work!" in {
      List(2,4,6) === removeOddNumbers(List(1,2,3,4,5,6))
    }
  }

  "flatMap" should {
    "return List(1,1,2,2,3,3) for flatMap(List(1,2,3))(i => List(i,i))" in {
      List(1,1,2,2,3,3) === flatMap(List(1,2,3))(i => List(i,i))
    }
  }

  "zipInts" should {
    "return List(5,7,9) from List(1,2,3) and List(4,5,6)" in {
      List(5,7,9) === zipInts(List(1,2,3),List(4,5,6))
    }
  }

  "zipInts" should {
    "return List(5,7) from List(1,2) and List(4,5,6)" in {
      List(5,7) === zipInts(List(1,2),List(4,5,6))
    }
  }

  "zipWith" should {
    "return List(5,7,9) for zipWith(List(1,2,3), List(4,5,6))(_ + _)" in {
      List(5,7,9) === zipWith(List(1,2,3), List(4,5,6))(_ + _)
    }

    "return List(5,7,9) for zipWith(List(1,2,3), List(4,5,6))(_ * _)" in {
      List(4,10,18) === zipWith(List(1,2,3), List(4,5,6))(_ * _)
    }

    "return List(\"1a\",\"2b\",\"3c\") for zipWith(List(1,2,3), List(\"a\",\"b\",\"c\"))(_.toString + _)" in {
      List("1a","2b","3c") === zipWith(List(1,2,3), List("a","b","c"))(_.toString + _)
    }
  }

  "hasSubsequence" should {
    "return true for List(1,2,3,4), List(1,2)" in {
      hasSubsequence(List(1,2,3,4), List(1,2)) === true
    }

    "return true for List(1,2,3,4), List(2,3)" in {
      hasSubsequence(List(1,2,3,4), List(2,3)) === true
    }

    "return true for List(1,2,3,4), List(3,4)" in {
      hasSubsequence(List(1,2,3,4), List(3,4)) === true
    }

    "return true for List(1,2,3,4), List(3)" in {
      hasSubsequence(List(1,2,3,4), List(3)) === true
    }

    "return true for List(1,2,3,4), List(4)" in {
      hasSubsequence(List(1,2,3,4), List(4)) === true
    }

    "return true for List(1,2,3,4), Nil" in {
      hasSubsequence(List(1,2,3,4), Nil) === true
    }

    "return false for List(1,2,3,4), List(3, 7)" in {
      hasSubsequence(List(1,2,3,4), List(4,5)) === false
    }

    "return false for List(1,2,3,4), List(4,5)" in {
      hasSubsequence(List(1,2,3,4), List(4,5)) === false
    }
  }
}
