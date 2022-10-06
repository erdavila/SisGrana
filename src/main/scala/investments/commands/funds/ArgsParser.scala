package sisgrana
package investments.commands.funds

import investments.ArgumentsParser
import java.io.PrintStream
import java.time.YearMonth

object ArgsParser extends ArgumentsParser[ParsedArgs] {
  private val AccumulatedOption = "--accumulated"
  private val AccumulatedShortOption = "--acc"
  private val TotalsOnlyOption = "--totals-only"
  private val NoTotalsOption = "--no-totals"

  override protected def spec: Parser[ParsedArgs] =
    for {
      accumulated <- takeOption(AccumulatedOption, AccumulatedShortOption)
      fundsAndTotals <- takeFundsAndTotalsOptions
      (funds, totals) = fundsAndTotals
      month <- takeMonth
    } yield ParsedArgs(
      month = month,
      accumulated = accumulated,
      funds = funds,
      totals = totals,
    )

  private def takeFundsAndTotalsOptions: Parser[(Boolean, Boolean)] =
    for {
      totalsOnly <- takeOption(TotalsOnlyOption)
      noTotals <- takeOption(NoTotalsOption)
      _ = if (totalsOnly && noTotals) error(s"As opções $TotalsOnlyOption e $NoTotalsOption são incompatíveis")
      funds = !totalsOnly
      totals = totalsOnly || !noTotals
      _ = assert(funds || totals)
    } yield (funds, totals)

  private def takeMonth: Parser[YearMonth] =
    for (str <- takeNext)
      yield YearMonth.parse(str)

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println(s"  [$AccumulatedOption|$AccumulatedShortOption] [$TotalsOnlyOption|$NoTotalsOption] ANO-MÊS")
  }
}
