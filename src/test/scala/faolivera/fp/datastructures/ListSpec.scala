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
}
