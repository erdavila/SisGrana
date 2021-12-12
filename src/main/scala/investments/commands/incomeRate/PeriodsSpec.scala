package sisgrana
package investments.commands.incomeRate

case class PeriodsSpec(firstPeriod: Period, offset: Option[Int], count: Int) {
  def periods: Iterator[Period] = {
    val offset = this.offset.getOrElse(firstPeriod.lengthInChronoUnits)
    Iterator.iterate(firstPeriod)(_.offset(offset)).take(count)
  }
}

object PeriodsSpec {
  def parse(string: String): PeriodsSpec =
    string match {
      case s"$periodString/$offsetString:$countString" if offsetString.lengthIs <= 3 =>
        PeriodsSpec(Period.parse(periodString), Some(offsetString.toInt), countString.toInt)
      case s"$periodString/$countString" if countString.lengthIs <= 3 =>
        PeriodsSpec(Period.parse(periodString), None, countString.toInt)
      case periodString =>
        PeriodsSpec(Period.parse(periodString), None, 1)
    }
}
