package sisgrana
package investments

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RateTest extends AnyFunSuite with Matchers {
  test("compose") {
    val rate1 = Rate(0.1234, 3)
    val rate2 = Rate(0.4321, 4)

    val result = Rate.compose(rate1, rate2)

    val expectedValue = (1 + rate1.value) * (1 + rate2.value) - 1
    result.value should be (expectedValue +- 1e-15)
    result.days should be (rate1.days + rate2.days)
  }

  test("repeat") {
    val rate = Rate(0.1234, 3)

    Rate.repeat(rate, 1) should be theSameInstanceAs rate

    for (times <- 2 to 100) {
      withClue(s"times = $times") {
        val result = Rate.repeat(rate, times)

        val expectedValue = math.pow(1 + rate.value, times) - 1
        result.value should be (expectedValue +- 1e-12)
        result.days should be (rate.days * times)
      }
    }
  }
}
