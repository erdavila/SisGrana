package sisgrana
package utils

class packageTest extends TestBase {
  test("sameNonZeroSigns()") {
    val TestedNumbers = Seq(-7, 0, +7)
    for {
      x <- TestedNumbers
      y <- TestedNumbers
      expectedOutput = x != 0 && y != 0 && (x > 0 == y > 0)
    } withClue(s"x=$x, y=$y") {
      sameNonZeroSigns(x, y) should be (expectedOutput)
    }
  }

  test("oppositeSigns()") {
    val TestedNumbers = Seq(-7, 0, +7)
    for {
      x <- TestedNumbers
      y <- TestedNumbers
      expectedOutput = x != 0 && y != 0 && (x > 0 != y > 0)
    } withClue(s"x=$x, y=$y") {
      oppositeSigns(x, y) should be(expectedOutput)
    }
  }
}
