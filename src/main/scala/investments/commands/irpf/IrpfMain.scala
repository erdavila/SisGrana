package sisgrana
package investments.commands.irpf

import investments.commands.assetsAtDate.AssetsAtDateMain
import investments.model._
import investments.model.ctx._
import investments.{AssetType, Formatting, Taxation}
import java.time.{LocalDate, Month, Year, YearMonth}
import utils.{BrNumber, IndentedPrinter}

object IrpfMain {
  private val SwingTradeExemptableLimit = 20_000.00
  private val SwingTradeTaxRate = 0.15
  private val DayTradeTaxRate = 0.20
  private val FIIsTaxRate = 0.20

  def main(args: Array[String]): Unit = {
    val (showDetails, years) = processArgs(args)
    val main = new IrpfMain(showDetails)

    for (year <- years) {
      main.processYear(year)
    }
  }

  private def processArgs(args: Array[String]): (Boolean, Array[Year]) =
    args.foldLeft((false, Array.empty[Year])) { case ((showDetails, years), arg) =>
      if (arg == "--details") {
        (true, years)
      } else {
        (showDetails, years :+ Year.parse(arg))
      }
    }

  implicit private class IterableOps[A](private val iterable: Seq[A]) extends AnyVal {
    def groupAndSortBy[K: Ordering](f: A => K): Seq[(K, Seq[A])] =
      iterable.groupBy(f).toIndexedSeq.sortBy(_._1)
  }
}

class IrpfMain(showDetails: Boolean) extends LocalDateSupport {
  import IrpfMain._

  private val typeResolver = AssetType.Resolver.get()
  private val printer = new IndentedPrinter

  private def processYear(year: Year): Unit =
    printer.context(year) {
      val minDate = year.atDay(1)
      val maxDate = year.atMonth(Month.DECEMBER).atEndOfMonth()
      val assetPeriods = ctx.run {
        query[AssetPeriod]
          .filter(ap => ap.beginDate >= lift(minDate) && ap.beginDate <= lift(maxDate))
          .sortBy(_.beginDate)
      }

      for ((month, assetPeriods) <- assetPeriods.groupAndSortBy(_.beginDate.getMonth)) {
        processYearMonth(year.atMonth(month), assetPeriods)
      }

      AssetsAtDateMain.showAssetsAtDate(maxDate, printer)
    }

  private case class TradesResults(
    stocksDayTrade: TradeResult,
    optionsDayTrade: TradeResult,
    exemptableSwingTrade: TradeResult,
    totalExemptableSwingTradeSalesValueWithoutCost: Double,
    nonExemptableStocksSwingTrade: TradeResult,
    nonExemptableOptionsSwingTrade: TradeResult,
    fiisTrade: TradeResult,
  ) {
    def +(other: TradesResults): TradesResults =
      TradesResults(
        this.stocksDayTrade + other.stocksDayTrade,
        this.optionsDayTrade + other.optionsDayTrade,
        this.exemptableSwingTrade + other.exemptableSwingTrade,
        this.totalExemptableSwingTradeSalesValueWithoutCost + other.totalExemptableSwingTradeSalesValueWithoutCost,
        this.nonExemptableStocksSwingTrade + other.nonExemptableStocksSwingTrade,
        this.nonExemptableOptionsSwingTrade + other.nonExemptableOptionsSwingTrade,
        this.fiisTrade + other.fiisTrade,
      )

    lazy val totalDayTrade: TradeResult = stocksDayTrade + optionsDayTrade
    lazy val totalNonExemptableSwingTrade: TradeResult = nonExemptableStocksSwingTrade + nonExemptableOptionsSwingTrade
    lazy val totalSwingTrade: TradeResult = exemptableSwingTrade + totalNonExemptableSwingTrade
  }

  private object TradesResults {
    val Zero: TradesResults = TradesResults(TradeResult.Zero, TradeResult.Zero, TradeResult.Zero, 0.0, TradeResult.Zero, TradeResult.Zero, TradeResult.Zero)
  }

  private def processYearMonth(yearMonth: YearMonth, assetPeriods: Seq[AssetPeriod]): Unit =
    printer.context(yearMonth) {
      val tradeResults = assetPeriods.groupAndSortBy(_.beginDate.getDayOfMonth)
        .map { case (dayOfMonth, assetPeriods) =>
          processDate(yearMonth.atDay(dayOfMonth), assetPeriods)
        }
        .reduce(_ + _)

      if (tradeResults.stocksDayTrade.quantity != 0 || tradeResults.optionsDayTrade.quantity != 0) {
        printer.context("Day-Trade") {
          mayPrintResult("Ações", tradeResults.stocksDayTrade)
          mayPrintResult("Opções", tradeResults.optionsDayTrade)

          val result = printResult(tradeResults.totalDayTrade)
          val tax = result * DayTradeTaxRate
          if (tax > 0.0) {
            printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
          }
        }
      }

      if (tradeResults.totalSwingTrade.quantity != 0) {
        printer.context("Operações Comuns") {
          val exemptableSwingTradeTax =
            if (tradeResults.exemptableSwingTrade.quantity != 0) {
              printer.context("Isentáveis") {
                val totalSales = tradeResults.totalExemptableSwingTradeSalesValueWithoutCost
                printer.println(s"Total de vendas: ${BrNumber.formatMoney(totalSales)}")
                val result = tradeResults.exemptableSwingTrade.totalNetValue
                val isExempt = totalSales < SwingTradeExemptableLimit || result < 0.0
                val descr =
                  if (result <= 0) "prejuízo"
                  else if (isExempt) "lucro isento"
                  else "lucro tributável"
                printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
                if (isExempt) {
                  0.0
                } else {
                  val tax = result * SwingTradeTaxRate
                  printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
                  tax
                }
              }
            } else {
              0.0
            }

          val nonExemptableSwingTradeTax =
            if (tradeResults.nonExemptableStocksSwingTrade.quantity != 0 || tradeResults.nonExemptableOptionsSwingTrade.quantity != 0) {
              printer.context("Não isentáveis") {
                mayPrintResult("Ações", tradeResults.nonExemptableStocksSwingTrade)
                mayPrintResult("Opções", tradeResults.nonExemptableOptionsSwingTrade)

                val result = printResult(tradeResults.totalNonExemptableSwingTrade)
                val tax = result * SwingTradeTaxRate
                if (tax > 0.0) {
                  printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
                }
                tax
              }
            } else {
              0.0
            }

          printer.context("Totais") {
            val tax = exemptableSwingTradeTax + nonExemptableSwingTradeTax
            if (tax > 0.0) {
              printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
            }
          }
        }
      }

      if (tradeResults.fiisTrade.quantity != 0) {
        printer.context("FIIs") {
          val result = printResult(tradeResults.fiisTrade)
          val tax = result * FIIsTaxRate
          if (tax > 0.0) {
            printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
          }
        }
      }

      printer.println()
    }

  private def mayPrintResult(context: Any, tradeResult: TradeResult): Unit = {
    if (tradeResult.quantity != 0) {
      printer.context(context) {
        printResult(tradeResult)
      }
    }
  }

  private def printResult(tradeResult: TradeResult): Double = {
    val result = tradeResult.totalNetValue
    val descr =
      if (result <= 0.0) "prejuízo"
      else "lucro"
    printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
    result
  }

  private def processDate(date: LocalDate, assetPeriods: Seq[AssetPeriod]): TradesResults =
    printer.context(date, showDetails) {
      assetPeriods.groupAndSortBy(_.stockbroker)
        .map { case (stockbroker, assetPeriods) =>
          processStockbroker(stockbroker, assetPeriods)
        }
        .reduce(_ + _)
    }

  private def processStockbroker(stockbroker: String, assetPeriods: Seq[AssetPeriod]): TradesResults =
    printer.context(stockbroker, showDetails) {
      assetPeriods.sortBy(_.asset)
        .map { ap =>
          val detailsOpt = printer.hierarchy.optionalTree(ap.asset, showDetails)(
            Formatting.forAssetPeriod(ap, None): _*
          )

          for (details <- detailsOpt) {
            printer.hierarchy.print(details)
          }

          val assetType = typeResolver(ap.asset)

          val (stocksDayTrade, optionsDayTrade) =
            if (assetType == AssetType.Option) (TradeResult.Zero, ap.dayTradeResult)
            else (ap.dayTradeResult, TradeResult.Zero)

          assetType.taxation match {
            case Taxation.ExemptableSwingTrade =>
              TradesResults(
                stocksDayTrade = stocksDayTrade,
                optionsDayTrade = optionsDayTrade,
                exemptableSwingTrade = ap.swingTradeResult,
                totalExemptableSwingTradeSalesValueWithoutCost = ap.swingTradeResult.totalSaleValue,
                nonExemptableStocksSwingTrade = TradeResult.Zero,
                nonExemptableOptionsSwingTrade = TradeResult.Zero,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.NonExemptableStocksSwingTrade =>
              TradesResults(
                stocksDayTrade = stocksDayTrade,
                optionsDayTrade = optionsDayTrade,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableStocksSwingTrade = ap.swingTradeResult,
                nonExemptableOptionsSwingTrade = TradeResult.Zero,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.NonExemptableOptionsSwingTrade =>
              TradesResults(
                stocksDayTrade = stocksDayTrade,
                optionsDayTrade = optionsDayTrade,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableStocksSwingTrade = TradeResult.Zero,
                nonExemptableOptionsSwingTrade = ap.swingTradeResult,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.FII =>
              TradesResults(
                stocksDayTrade = stocksDayTrade,
                optionsDayTrade = optionsDayTrade,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableStocksSwingTrade = TradeResult.Zero,
                nonExemptableOptionsSwingTrade = TradeResult.Zero,
                fiisTrade = ap.swingTradeResult,
              )
            case Taxation.NonVariableIncome => TradesResults.Zero
          }
        }
        .reduce(_ + _)
    }
}
