package sisgrana
package investments.commands.funds

import investments.ArgumentsParser
import java.io.PrintStream
import java.time.YearMonth

object ArgsParser extends ArgumentsParser[ParsedArgs] {
  private val AccumulatedOption = "--accumulated"
  private val AccumulatedShortOption = "--acc"
  private val TotalsOnlyOption = "--totals-only"

  override protected def spec: Parser[ParsedArgs] =
    for {
      accumulated <- takeOption(AccumulatedOption, AccumulatedShortOption)
      totalsOnly <- takeOption(TotalsOnlyOption)
      month <- takeMonth
    } yield ParsedArgs(
      month = month,
      accumulated = accumulated,
      totalsOnly = totalsOnly,
    )

  private def takeMonth: Parser[YearMonth] =
    for (str <- takeNext)
      yield YearMonth.parse(str)

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println(s"  [$AccumulatedOption|$AccumulatedShortOption] [$TotalsOnlyOption] ANO-MÊS")
  }
}
