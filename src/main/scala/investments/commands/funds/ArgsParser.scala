package sisgrana
package investments.commands.funds

import investments.ArgumentsParser
import investments.commands.funds.operations.list.ListChunkMaker
import java.io.PrintStream
import java.time.YearMonth

object ArgsParser extends ArgumentsParser[OperationArguments] {
  private val ListOperation = "list"
  private val InitOperation = "init"
  private val EvolutionOfOperation = "evolution-of"

  private val TotalsOnlyOption = "--totals-only"
  private val NoTotalsOption = "--no-totals"
  private val SummaryOnlyOption = "--summary-only"
  private val NoSummaryOption = "--no-summary"
  private val PositiveFilterOption = "--filter"
  private val NegativeFilterOption = "--filter-not"
  private val MaxNameLenOption = "--max-name-len"

  private val DefaultMaxNameLen = 10
  private val MinNameLenLimit = 4

  private case class DetailsAndAggregationOptions(details: Boolean, aggregation: Boolean)

  override protected def spec: Parser[OperationArguments] =
    for {
      operation <- takeNextIf(operation => operation == ListOperation || operation == InitOperation || operation == EvolutionOfOperation)
      opArgs <-
        operation match {
          case Some(InitOperation) => takeInitOpArgs
          case Some(EvolutionOfOperation) => takeEvolutionOfOpArgs
          case _ => takeListOpArgs
        }
    } yield opArgs

  private def takeListOpArgs: Parser[OperationArguments.List] =
    for {
      fundsAndTotals <- takeFundsAndTotalsOptions
      daysAndSummary <- takeDaysAndSummaryOptions
      positiveFilters <- takeFilters(PositiveFilterOption)
      negativeFilters <- takeFilters(NegativeFilterOption)
      maxNameLen <- takeMaxNameLenOption
      monthRange <- takeMonthRange
    } yield OperationArguments.List(
      monthRange = monthRange,
      printOptions = ListChunkMaker.Options(
        funds = fundsAndTotals.details,
        totals = fundsAndTotals.aggregation,
        days = daysAndSummary.details,
        summary = daysAndSummary.aggregation,
        maxNameLen = maxNameLen,
      ),
      positiveFilters = positiveFilters,
      negativeFilters = negativeFilters,
    )

  private def takeInitOpArgs: Parser[OperationArguments.Init] =
    for (month <- takeMonth)
      yield OperationArguments.Init(month)

  private def takeEvolutionOfOpArgs: Parser[OperationArguments.EvolutionOf] =
    for {
      fund <- takeNext
      monthRange <- takeMonthRange
    } yield OperationArguments.EvolutionOf(
      fund = fund,
      monthRange = monthRange,
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

  private def takeFilters(forms: String*): Parser[Seq[String]] =
    takeOptionParameter(forms: _*) $ {
      case Some(option) => option.split(",").toSeq
      case None => Seq.empty
    }

  private def takeMaxNameLenOption: Parser[Int] =
    for {
      maxNameLenStr <- takeOptionParameter(MaxNameLenOption)
      maxNameLen = maxNameLenStr.fold(DefaultMaxNameLen) {
        _.toIntOption.filter(_ >= MinNameLenLimit).getOrElse(error(s"Valor inválido para a opção $MaxNameLenOption"))
      }
    } yield maxNameLen

  private def takeMonth: Parser[YearMonth] =
    for (str <- takeNext)
      yield YearMonth.parse(str)

  private def takeMonthRange: Parser[MonthRange] =
    for (str <- takeNext)
      yield parseMonthRange(str)

  private def parseMonthRange(string: String): MonthRange =
    string match {
      case s"$initialMonthString:$finalMonthString" =>
        val initialMonth = YearMonth.parse(initialMonthString)
        val finalMonth = YearMonth.parse(finalMonthString)
        if (initialMonth `isAfter` finalMonth) {
          error("O ANO-MÊS-INICIAL não pode ser posterior ao ANO-MÊS-FINAL")
        }
        MonthRange(initialMonth, finalMonth)
      case s =>
        val month = YearMonth.parse(s)
        MonthRange(month, month)
    }

  private def incompatibleOptions(option1: String, option2: String): Nothing =
    error(s"As opções $option1 e $option2 são incompatíveis")

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println(s"  [$ListOperation] [OPÇÕES] MESES")
    printStream.println(s"  $InitOperation ANO-MÊS")
    printStream.println(s"  $EvolutionOfOperation NOME MESES")
    printStream.println()
    printStream.println(s"  OPÇÕES podem ser:")
    printStream.println(s"    $TotalsOnlyOption|$NoTotalsOption")
    printStream.println(s"    $SummaryOnlyOption|$NoSummaryOption")
    printStream.println(s"    $PositiveFilterOption NOME,...")
    printStream.println(s"    $NegativeFilterOption NOME,...")
    printStream.println(s"    $MaxNameLenOption TAMANHO (default é $DefaultMaxNameLen)")
    printStream.println()
    printStream.println(s"  MESES pode ser:")
    printStream.println(s"    ANO-MÊS (ex.: 2022-08)")
    printStream.println(s"    ANO-MÊS-INICIAL:ANO-MÊS-FINAL (ex.: 2022-07:2022-09)")
  }
}
