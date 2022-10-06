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
  private val SummaryOnlyOption = "--summary-only"
  private val NoSummaryOption = "--no-summary"
  private val PositiveFilterOption = "--filter"
  private val NegativeFilterOption = "--filter-not"

  private case class DetailsAndAggregationOptions(details: Boolean, aggregation: Boolean)

  override protected def spec: Parser[ParsedArgs] =
    for {
      accumulated <- takeOption(AccumulatedOption, AccumulatedShortOption)
      fundsAndTotals <- takeFundsAndTotalsOptions
      daysAndSummary <- takeDaysAndSummaryOptions
      _ = if (accumulated && !daysAndSummary.details) incompatibleOptions(AccumulatedOption, SummaryOnlyOption)
      positiveFilters <- takeFilters(PositiveFilterOption)
      negativeFilters <- takeFilters(NegativeFilterOption)
      month <- takeMonth
    } yield ParsedArgs(
      month = month,
      printOptions = Printer.Options(
        accumulated = accumulated,
        funds = fundsAndTotals.details,
        totals = fundsAndTotals.aggregation,
        days = daysAndSummary.details,
        summary = daysAndSummary.aggregation,
      ),
      positiveFilters = positiveFilters,
      negativeFilters = negativeFilters,
    )

  private def takeFundsAndTotalsOptions: Parser[DetailsAndAggregationOptions] =
    takeDetailsAndAggregationOptions(Seq(TotalsOnlyOption), Seq(NoTotalsOption))

  private def takeDaysAndSummaryOptions: Parser[DetailsAndAggregationOptions] =
    takeDetailsAndAggregationOptions(Seq(SummaryOnlyOption), Seq(NoSummaryOption))

  private def takeDetailsAndAggregationOptions(aggregationOnlyForms: Seq[String], noAggregationForms: Seq[String]): Parser[DetailsAndAggregationOptions] =
    for {
      aggregationOnly <- takeOption(aggregationOnlyForms: _*)
      noAggregation <- takeOption(noAggregationForms: _*)
      _ = if (aggregationOnly && noAggregation) incompatibleOptions(aggregationOnlyForms.head, noAggregationForms.head)
      details = !aggregationOnly
      aggregation = aggregationOnly || !noAggregation
      _ = assert(details || aggregation)
    } yield DetailsAndAggregationOptions(details, aggregation)

  private def incompatibleOptions(option1: String, option2: String): Nothing =
    error(s"As opções ${option1} e ${option2} são incompatíveis")

  private def takeFilters(forms: String*): Parser[Seq[String]] =
    takeOptionParameter(forms: _*) $ {
      case Some(option) => option.split(",").toSeq
      case None => Seq.empty
    }

  private def takeMonth: Parser[YearMonth] =
    for (str <- takeNext)
      yield YearMonth.parse(str)

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados: OPÇÕES ANO-MÊS")
    printStream.println()
    printStream.println(s"  OPÇÕES:")
    printStream.println(s"    $AccumulatedOption|$AccumulatedShortOption")
    printStream.println(s"    $TotalsOnlyOption|$NoTotalsOption")
    printStream.println(s"    $SummaryOnlyOption|$NoSummaryOption")
    printStream.println(s"    $PositiveFilterOption NOME,...")
    printStream.println(s"    $NegativeFilterOption NOME,...")
  }
}
