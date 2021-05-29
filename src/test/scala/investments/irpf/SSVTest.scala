package sisgrana
package investments.irpf

import sisgrana.investments.variableIncome.fileImport.SSV.{InterruptedContentException, UnexpectedCharacterException}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sisgrana.investments.variableIncome.fileImport.SSV

class SSVTest extends AnyFunSuite with Matchers {
  private def fromChars(chars: String): Seq[Seq[String]] =
    SSV.fromChars(chars.iterator)

  test("simple case") {
    val allLinesValues = fromChars(
      """A B
        |#x
        |C D
        |""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B"), Seq("C", "D")))
  }

  test("simple case with more chars") {
    val allLinesValues = fromChars(
      """  A1  B2  |
        |# x  |
        |  C3  D4  |
        |""".stripMargin.replace("|", "")
    )

    allLinesValues should be (Seq(Seq("A1", "B2"), Seq("C3", "D4")))
  }

  test("line without chars") {
    val allLinesValues = fromChars(
      """A B
        |
        |C D
        |""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B"), Seq(), Seq("C", "D")))
  }

  test("line with spaces only") {
    val allLinesValues = fromChars(
      """A B
        |   |
        |C D
        |""".stripMargin.replace("|", "")
    )

    allLinesValues should be (Seq(Seq("A", "B"), Seq(), Seq("C", "D")))
  }

  test("quoted value") {
    val allLinesValues = fromChars(
      """A "B C" D
        |""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B C", "D")))
  }

  test("empty value") {
    val allLinesValues = fromChars(
      """A "" B
        |""".stripMargin.replace("|", "")
    )

    allLinesValues should be (Seq(Seq("A", "", "B")))
  }

  test("quoted line break") {
    val allLinesValues = fromChars(
      """A "B |
        | C" D
        |""".stripMargin.replace("|", "")
    )

    allLinesValues should be (Seq(Seq("A", "B \n C", "D")))
  }

  test("last line without line break") {
    val allLinesValues = fromChars(
      """A B
        |C D""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B"), Seq("C", "D")))
  }

  test("error location in first line") {
    val thrown = the [UnexpectedCharacterException] thrownBy {
      fromChars(
        """A"B
          |""".stripMargin
      )
    }

    thrown.char should be ('B')
    thrown.lineNumber should be (1)
    thrown.columnNumber should be (3)
  }

  test("error location in second line") {
    val thrown = the [UnexpectedCharacterException] thrownBy {
      fromChars(
        """A B
          |C"D
          |""".stripMargin
      )
    }

    thrown.char should be ('D')
    thrown.lineNumber should be (2)
    thrown.columnNumber should be (3)
  }

  test("interrupted quoted value") {
    an [InterruptedContentException] should be thrownBy {
      fromChars(
        """A "B""".stripMargin
      )
    }
  }

  test("first value in line is quoted") {
    val allLinesValues = fromChars(
      """"A" B""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B")))
  }

  test("last value in line is quoted") {
    val allLinesValues = fromChars(
      """A "B"
        |""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B")))
  }

  test("last value in line is quoted, without line break") {
    val allLinesValues = fromChars(
      """A "B"""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B")))
  }

  test("escaped quote in value") {
    val allLinesValues = fromChars(
      """A B""C D
        |""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B\"C", "D")))
  }

  test("escaped quote in quoted value") {
    val allLinesValues = fromChars(
      """A "B""C" D
        |""".stripMargin
    )

    allLinesValues should be (Seq(Seq("A", "B\"C", "D")))
  }

  test("non-escaped quote in value") {
    val thrown = the [UnexpectedCharacterException] thrownBy {
      fromChars(
        """A B" D"""
      )
    }

    thrown.char should be (' ')
    thrown.lineNumber should be (1)
    thrown.columnNumber should be (5)
  }

  test("interrupted in non-escaped quote") {
    an [InterruptedContentException] should be thrownBy {
      fromChars(
        """A B"""".stripMargin
      )
    }
  }

  test("last line ending in spaces without line break") {
    val allLinesValues = fromChars(
      """A
        |B  """.stripMargin
    )

    allLinesValues should be (Seq(Seq("A"), Seq("B")))
  }

  test("last line is comment without line break") {
    val allLinesValues = fromChars(
      """A
        |# B """.stripMargin
    )

    allLinesValues should be (Seq(Seq("A")))
  }
}
