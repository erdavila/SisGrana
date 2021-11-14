package sisgrana
package investments.irpf

import investments.assetsAtDate.AssetsAtDateMain
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

  private val typeResolver = AssetType.Resolver.instance
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
    dayTrade: TradeResult,
    exemptableSwingTrade: TradeResult,
    totalExemptableSwingTradeSalesValueWithoutCost: Double,
    nonExemptableSwingTrade: TradeResult,
    fiisTrade: TradeResult,
  ) {
    def +(other: TradesResults): TradesResults =
      TradesResults(
        this.dayTrade + other.dayTrade,
        this.exemptableSwingTrade + other.exemptableSwingTrade,
        this.totalExemptableSwingTradeSalesValueWithoutCost + other.totalExemptableSwingTradeSalesValueWithoutCost,
        this.nonExemptableSwingTrade + other.nonExemptableSwingTrade,
        this.fiisTrade + other.fiisTrade,
      )

    lazy val totalSwingTrade: TradeResult = exemptableSwingTrade + nonExemptableSwingTrade
  }

  private object TradesResults {
    val Zero: TradesResults = TradesResults(TradeResult.Zero, TradeResult.Zero, 0.0, TradeResult.Zero, TradeResult.Zero)
  }

  private def processYearMonth(yearMonth: YearMonth, assetPeriods: Seq[AssetPeriod]): Unit =
    printer.context(yearMonth) {
      val tradeResults = assetPeriods.groupAndSortBy(_.beginDate.getDayOfMonth)
        .map { case (dayOfMonth, assetPeriods) =>
          processDate(yearMonth.atDay(dayOfMonth), assetPeriods)
        }
        .reduce(_ + _)

      if (tradeResults.dayTrade.quantity != 0) {
        printer.context("Day-Trade") {
          val result = tradeResults.dayTrade.totalNetValue
          val descr =
            if (result <= 0.0) "prejuízo"
            else "lucro"
          printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
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
            if (tradeResults.nonExemptableSwingTrade.quantity != 0) {
              printer.context("Não isentáveis") {
                val result = tradeResults.nonExemptableSwingTrade.totalNetValue
                val descr =
                  if (result <= 0.0) "prejuízo"
                  else "lucro"
                printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
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
            val result = tradeResults.totalSwingTrade.totalNetValue
            val descr =
              if (result <= 0.0) "prejuízo"
              else "lucro"
            printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
            val tax = exemptableSwingTradeTax + nonExemptableSwingTradeTax
            if (tax > 0.0) {
              printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
            }
          }
        }
      }

      if (tradeResults.fiisTrade.quantity != 0) {
        printer.context("FIIs") {
          val result = tradeResults.fiisTrade.totalNetValue
          val descr =
            if (result <= 0.0) "prejuízo"
            else "lucro"
          printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
          val tax = result * FIIsTaxRate
          if (tax > 0.0) {
            printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
          }
        }
      }

      printer.println()
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

          typeResolver(ap.asset).taxation match {
            case Taxation.ExemptableSwingTrade =>
              TradesResults(
                dayTrade = ap.dayTradeResult,
                exemptableSwingTrade = ap.swingTradeResult,
                totalExemptableSwingTradeSalesValueWithoutCost = ap.swingTradeResult.totalSaleValue,
                nonExemptableSwingTrade = TradeResult.Zero,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.NonExemptableSwingTrade =>
              TradesResults(
                dayTrade = ap.dayTradeResult,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableSwingTrade = ap.swingTradeResult,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.FII =>
              TradesResults(
                dayTrade = ap.dayTradeResult,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableSwingTrade = TradeResult.Zero,
                fiisTrade = ap.swingTradeResult,
              )
            case Taxation.NonVariableIncome => TradesResults.Zero
          }
        }
        .reduce(_ + _)
    }
}
