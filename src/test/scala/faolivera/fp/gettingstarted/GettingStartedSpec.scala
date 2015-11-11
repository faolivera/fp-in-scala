package faolivera.fp.gettingstarted

import org.specs2.mutable.Specification

/**
 * Created by folivera on 11/11/15.
 */
class GettingStartedSpec extends Specification {
  import GettingStarted._
  import LoopsFunctional._

  "Fib" should {
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

}
