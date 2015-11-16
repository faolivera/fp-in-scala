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




}
