package sisgrana
package investments.commands.funds

import investments.ArgumentsParser
import java.io.PrintStream
import java.time.YearMonth

object ArgsParser extends ArgumentsParser[OperationArguments] {
  private val ListOperation = "list"
  private val InitOperation = "init"

  private val TotalsOnlyOption = "--totals-only"
  private val NoTotalsOption = "--no-totals"
  private val SummaryOnlyOption = "--summary-only"
  private val NoSummaryOption = "--no-summary"
  private val PositiveFilterOption = "--filter"
  private val NegativeFilterOption = "--filter-not"

  private case class DetailsAndAggregationOptions(details: Boolean, aggregation: Boolean)

  override protected def spec: Parser[OperationArguments] =
    for {
      operation <- takeNextIf(operation => operation == ListOperation || operation == InitOperation)
      opArgs <-
        if (operation.contains(InitOperation)) takeInitOpArgs
        else takeListOpArgs
    } yield opArgs

  private def takeListOpArgs: Parser[OperationArguments.List] =
    for {
      fundsAndTotals <- takeFundsAndTotalsOptions
      daysAndSummary <- takeDaysAndSummaryOptions
      positiveFilters <- takeFilters(PositiveFilterOption)
      negativeFilters <- takeFilters(NegativeFilterOption)
      monthRange <- takeMonthRange
    } yield OperationArguments.List(
      initialMonth = monthRange._1,
      finalMonth = monthRange._2,
      printOptions = ChunkMaker.Options(
        funds = fundsAndTotals.details,
        totals = fundsAndTotals.aggregation,
        days = daysAndSummary.details,
        summary = daysAndSummary.aggregation,
      ),
      positiveFilters = positiveFilters,
      negativeFilters = negativeFilters,
    )

  private def takeInitOpArgs: Parser[OperationArguments.Init] =
    for (month <- takeMonth)
      yield OperationArguments.Init(month)

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

  private def takeFilters(forms: String*): Parser[Seq[String]] =
    takeOptionParameter(forms: _*) $ {
      case Some(option) => option.split(",").toSeq
      case None => Seq.empty
    }

  private def takeMonth: Parser[YearMonth] =
    for (str <- takeNext)
      yield YearMonth.parse(str)

  private def takeMonthRange: Parser[(YearMonth, YearMonth)] =
    for (str <- takeNext)
      yield parseMonthRange(str)

  private def parseMonthRange(string: String): (YearMonth, YearMonth) =
    string match {
      case s"$initialMonthString:$finalMonthString" =>
        val initialMonth = YearMonth.parse(initialMonthString)
        val finalMonth = YearMonth.parse(finalMonthString)
        if (initialMonth `isAfter` finalMonth) {
          error("O ANO-MÊS-INICIAL não pode ser posterior ao ANO-MÊS-FINAL")
        }
        (initialMonth, finalMonth)
      case s =>
        val month = YearMonth.parse(s)
        (month, month)
    }

  private def incompatibleOptions(option1: String, option2: String): Nothing =
    error(s"As opções $option1 e $option2 são incompatíveis")

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println("  [list] [OPÇÕES] MESES")
    printStream.println("  init ANO-MÊS")
    printStream.println()
    printStream.println(s"  OPÇÕES podem ser:")
    printStream.println(s"    $TotalsOnlyOption|$NoTotalsOption")
    printStream.println(s"    $SummaryOnlyOption|$NoSummaryOption")
    printStream.println(s"    $PositiveFilterOption NOME,...")
    printStream.println(s"    $NegativeFilterOption NOME,...")
    printStream.println()
    printStream.println(s"  MESES pode ser:")
    printStream.println(s"    ANO-MÊS (ex.: 2022-08)")
    printStream.println(s"    ANO-MÊS-INICIAL:ANO-MÊS-FINAL (ex.: 2022-07:2022-09)")
  }
}
