package sisgrana
package investments.commands.incomeRate

import java.time.{LocalDate, Year, YearMonth}

class PeriodsSpecTest extends TestBase {
  test(".parse() + .periods") {
    val cases = Table(
      "string" -> "expected spec",

      "2000" -> PeriodsSpec(Period.Year(Year.of(2000)), None, 1),
      "2000/4" -> PeriodsSpec(Period.Year(Year.of(2000)), None, 4),
      "2000/3:4" -> PeriodsSpec(Period.Year(Year.of(2000)), Some(3), 4),

      "2000:2001" -> PeriodsSpec(Period.YearRange(Year.of(2000), Year.of(2001)), None, 1),
      "2000:2001/4" -> PeriodsSpec(Period.YearRange(Year.of(2000), Year.of(2001)), None, 4),
      "2000:2001/3:4" -> PeriodsSpec(Period.YearRange(Year.of(2000), Year.of(2001)), Some(3), 4),

      "2000-01" -> PeriodsSpec(Period.Month(YearMonth.of(2000, 1)), None, 1),
      "2000-01/4" -> PeriodsSpec(Period.Month(YearMonth.of(2000, 1)), None, 4),
      "2000-01/3:4" -> PeriodsSpec(Period.Month(YearMonth.of(2000, 1)), Some(3), 4),

      "2000-01:2000-02" -> PeriodsSpec(Period.MonthRange(YearMonth.of(2000, 1), YearMonth.of(2000, 2)), None, 1),
      "2000-01:2000-02/4" -> PeriodsSpec(Period.MonthRange(YearMonth.of(2000, 1), YearMonth.of(2000, 2)), None, 4),
      "2000-01:2000-02/3:4" -> PeriodsSpec(Period.MonthRange(YearMonth.of(2000, 1), YearMonth.of(2000, 2)), Some(3), 4),

      "2000-01-01:2000-01-03" -> PeriodsSpec(Period.DateRange(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 1, 3)), None, 1),
      "2000-01-01:2000-01-03/4" -> PeriodsSpec(Period.DateRange(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 1, 3)), None, 4),
      "2000-01-01:2000-01-03/3:4" -> PeriodsSpec(Period.DateRange(LocalDate.of(2000, 1, 1), LocalDate.of(2000, 1, 3)), Some(3), 4),
    )

    forAll(cases) { case (string, expectedSpec) =>
      PeriodsSpec.parse(string) should equal (expectedSpec)
    }
  }

  test(".periods") {
    val cases = Table(
      "spec" -> "expected periods",
      PeriodsSpec(Period.Year(Year.of(2021)), None, 3) -> Seq(
        Period.Year(Year.of(2021)),
        Period.Year(Year.of(2022)),
        Period.Year(Year.of(2023)),
      ),
      PeriodsSpec(Period.Year(Year.of(2021)), Some(1), 3) -> Seq(
        Period.Year(Year.of(2021)),
        Period.Year(Year.of(2022)),
        Period.Year(Year.of(2023)),
      ),
      PeriodsSpec(Period.Year(Year.of(2021)), Some(2), 3) -> Seq(
        Period.Year(Year.of(2021)),
        Period.Year(Year.of(2023)),
        Period.Year(Year.of(2025)),
      ),

      PeriodsSpec(Period.YearRange(Year.of(2000), Year.of(2002)), None, 3) -> Seq(
        Period.YearRange(Year.of(2000), Year.of(2002)),
        Period.YearRange(Year.of(2003), Year.of(2005)),
        Period.YearRange(Year.of(2006), Year.of(2008)),
      ),
      PeriodsSpec(Period.YearRange(Year.of(2000), Year.of(2002)), Some(3), 3) -> Seq(
        Period.YearRange(Year.of(2000), Year.of(2002)),
        Period.YearRange(Year.of(2003), Year.of(2005)),
        Period.YearRange(Year.of(2006), Year.of(2008)),
      ),
      PeriodsSpec(Period.YearRange(Year.of(2000), Year.of(2002)), Some(2), 3) -> Seq(
        Period.YearRange(Year.of(2000), Year.of(2002)),
        Period.YearRange(Year.of(2002), Year.of(2004)),
        Period.YearRange(Year.of(2004), Year.of(2006)),
      ),

      PeriodsSpec(Period.Month(YearMonth.of(2021, 1)), None, 3) -> Seq(
        Period.Month(YearMonth.of(2021, 1)),
        Period.Month(YearMonth.of(2021, 2)),
        Period.Month(YearMonth.of(2021, 3)),
      ),
      PeriodsSpec(Period.Month(YearMonth.of(2021, 1)), Some(1), 3) -> Seq(
        Period.Month(YearMonth.of(2021, 1)),
        Period.Month(YearMonth.of(2021, 2)),
        Period.Month(YearMonth.of(2021, 3)),
      ),
      PeriodsSpec(Period.Month(YearMonth.of(2021, 1)), Some(2), 3) -> Seq(
        Period.Month(YearMonth.of(2021, 1)),
        Period.Month(YearMonth.of(2021, 3)),
        Period.Month(YearMonth.of(2021, 5)),
      ),

      PeriodsSpec(Period.MonthRange(YearMonth.of(2021, 1), YearMonth.of(2021, 3)), None, 3) -> Seq(
        Period.MonthRange(YearMonth.of(2021, 1), YearMonth.of(2021, 3)),
        Period.MonthRange(YearMonth.of(2021, 4), YearMonth.of(2021, 6)),
        Period.MonthRange(YearMonth.of(2021, 7), YearMonth.of(2021, 9)),
      ),
      PeriodsSpec(Period.MonthRange(YearMonth.of(2021, 1), YearMonth.of(2021, 3)), Some(3), 3) -> Seq(
        Period.MonthRange(YearMonth.of(2021, 1), YearMonth.of(2021, 3)),
        Period.MonthRange(YearMonth.of(2021, 4), YearMonth.of(2021, 6)),
        Period.MonthRange(YearMonth.of(2021, 7), YearMonth.of(2021, 9)),
      ),
      PeriodsSpec(Period.MonthRange(YearMonth.of(2021, 1), YearMonth.of(2021, 3)), Some(2), 3) -> Seq(
        Period.MonthRange(YearMonth.of(2021, 1), YearMonth.of(2021, 3)),
        Period.MonthRange(YearMonth.of(2021, 3), YearMonth.of(2021, 5)),
        Period.MonthRange(YearMonth.of(2021, 5), YearMonth.of(2021, 7)),
      ),

      PeriodsSpec(Period.DateRange(LocalDate.of(2021, 1, 1), LocalDate.of(2021, 1, 5)), None, 3) -> Seq(
        Period.DateRange(LocalDate.of(2021, 1, 1), LocalDate.of(2021, 1, 5)),
        Period.DateRange(LocalDate.of(2021, 1, 5), LocalDate.of(2021, 1, 9)),
        Period.DateRange(LocalDate.of(2021, 1, 9), LocalDate.of(2021, 1, 13)),
      ),
      PeriodsSpec(Period.DateRange(LocalDate.of(2021, 1, 1), LocalDate.of(2021, 1, 5)), Some(4), 3) -> Seq(
        Period.DateRange(LocalDate.of(2021, 1, 1), LocalDate.of(2021, 1, 5)),
        Period.DateRange(LocalDate.of(2021, 1, 5), LocalDate.of(2021, 1, 9)),
        Period.DateRange(LocalDate.of(2021, 1, 9), LocalDate.of(2021, 1, 13)),
      ),
      PeriodsSpec(Period.DateRange(LocalDate.of(2021, 1, 1), LocalDate.of(2021, 1, 5)), Some(7), 3) -> Seq(
        Period.DateRange(LocalDate.of(2021, 1, 1), LocalDate.of(2021, 1, 5)),
        Period.DateRange(LocalDate.of(2021, 1, 8), LocalDate.of(2021, 1, 12)),
        Period.DateRange(LocalDate.of(2021, 1, 15), LocalDate.of(2021, 1, 19)),
      ),
    )

    forAll(cases) { case (spec, expectedPeriods) =>
      spec.periods.toSeq should equal (expectedPeriods)
    }
  }
}
