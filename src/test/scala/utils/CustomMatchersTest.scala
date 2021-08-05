package sisgrana
package utils

import org.scalatest.exceptions.TestFailedException

class CustomMatchersTest extends TestBase {
  test("subsetOf") {
    val small = Set(1)
    val big = Set(1, 2)

    noException should be thrownBy {
      small shouldBe subsetOf (big)
      big should not be subsetOf (small)
    }

    a [TestFailedException] shouldBe thrownBy {
      big shouldBe subsetOf (small)
    }

    a [TestFailedException] shouldBe thrownBy {
      small should not be subsetOf (big)
    }
  }
}
