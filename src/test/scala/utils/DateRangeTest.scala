package sisgrana
package utils

import java.time.LocalDate
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.language.implicitConversions

class DateRangeTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  import DateRangeTest.int2Date

  test(".size") {
    withClue("DayChange mode") {
      import DateRange.Mode.DayChange
      DateRange(1, 1).size should equal (0)
      DateRange(1, 2).size should equal (1)
      DateRange(1, 3).size should equal (2)
    }
    withClue("FullDay mode") {
      import DateRange.Mode.FullDay
      DateRange(1, 1).size should equal (1)
      DateRange(1, 2).size should equal (2)
      DateRange(1, 3).size should equal (3)
    }
  }
}

object DateRangeTest {
  implicit def int2Date(int: Int): LocalDate =
    LocalDate.of(2021, 8, 20).plusDays(int)

  implicit def int2DateRange(int: Int): DateRange =
    DateRange(int, int)
}
