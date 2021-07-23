package sisgrana
package investments.variableIncome.irpf

import investments.utils.BrNumber
import investments.variableIncome.model._
import investments.variableIncome.model.ctx._
import investments.variableIncome.{AssetType, Taxation, assetsAtDate}
import java.time.{LocalDate, Month, Year, YearMonth}
import utils.IndentedPrinter

object Main {
  private val SwingTradeExemptableLimit = 20_000.00
  private val SwingTradeTaxRate = 0.15
  private val DayTradeTaxRate = 0.20
  private val FIIsTaxRate = 0.20

  def main(args: Array[String]): Unit = {
    val (showDetails, years) = processArgs(args)
    val main = new Main(showDetails)

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

  private[irpf] def formatPositionChangeRaw(from: AmountWithCost, change: AmountWithCost, to: AmountWithCost): String = {
    val formattedFrom = from.signedFormatParens0
    val sign = if (change.signedQuantity < 0) '-' else '+'
    val formattedChange =
      if (from.quantity == 0 || math.signum(from.signedQuantity) == math.signum(change.signedQuantity)) {
        change.formatParens0
      } else {
        s"${change.quantity} unidades"
      }

    val formattedTo = to.signedFormatParens0
    s"$formattedFrom $sign $formattedChange = $formattedTo"
  }

  implicit private class IterableOps[A](private val iterable: Seq[A]) extends AnyVal {
    def groupAndSortBy[K: Ordering](f: A => K): Seq[(K, Seq[A])] =
      iterable.groupBy(f).toIndexedSeq.sortBy(_._1)
  }

  implicit private class AmountWithCostOps(private val amount: AmountWithCost) extends AnyVal {
    def format: String =
      formatValues(amount.quantity, amount.averagePriceWithCost, amount.totalValueWithCost)

    def formatParens0: String =
      parens0(amount.quantity, format)

    def signedFormat: String =
      formatValues(amount.signedQuantity, amount.averagePriceWithCost, amount.signedTotalValueWithCost)

    def signedFormatParens0: String =
      parens0(amount.quantity, signedFormat)

    private def parens0(quantity: Int, formatted: String) = if (quantity == 0) formatted else s"($formatted)"

    private def formatValues(quantity: Int, averagePriceWithCost: Double, totalValueWithCost: Double) =
      if (quantity == 0) {
        "0"
      } else {
        s"$quantity x ${averagePriceWithCost.format} = ${totalValueWithCost.format}"
      }
  }

  implicit private class TradeResultOps(private val tradeResult: TradeResult) extends AnyVal {
    def format: String =
      s"Resultado: (${tradeResult.saleAmount.format}) - (${tradeResult.purchaseAmount.format}) = ${tradeResult.totalNetValue.format}"
  }

  implicit private[irpf] class DoubleOps(private val double: Double) extends AnyVal {
    def format: String = BrNumber.formatMoney(double)
  }
}

class Main(showDetails: Boolean) extends LocalDateSupport {
  import Main._

  private val typeResolver = AssetType.Resolver.instance
  private val printer = new IndentedPrinter

  private def processYear(year: Year): Unit =
    printer.context(year) {
      val minDate = year.atDay(1)
      val maxDate = year.atMonth(Month.DECEMBER).atEndOfMonth()
      val assetChanges = ctx.run {
        query[AssetChange]
          .filter(ac => ac.date >= lift(minDate) && ac.date <= lift(maxDate))
          .sortBy(_.date)
      }

      for ((month, assetChanges) <- assetChanges.groupAndSortBy(_.date.getMonth)) {
        processYearMonth(year.atMonth(month), assetChanges)
      }

      assetsAtDate.Main.showAssetsAtDate(maxDate, printer)
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

  private def processYearMonth(yearMonth: YearMonth, assetChanges: Seq[AssetChange]): Unit =
    printer.context(yearMonth) {
      val tradeResults = assetChanges.groupAndSortBy(_.date.getDayOfMonth)
        .map { case (dayOfMonth, assetChanges) =>
          processDate(yearMonth.atDay(dayOfMonth), assetChanges)
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

  private def processDate(date: LocalDate, assetChanges: Seq[AssetChange]): TradesResults =
    printer.context(date, showDetails) {
      assetChanges.groupAndSortBy(_.stockbroker)
        .map { case (stockbroker, assetChanges) =>
          processStockbroker(stockbroker, assetChanges)
        }
        .reduce(_ + _)
    }

  private def processStockbroker(stockbroker: String, assetChanges: Seq[AssetChange]): TradesResults =
    printer.context(stockbroker, showDetails) {
      assetChanges.sortBy(_.asset)
        .map { ac =>
          def tradeResultDetailsOpt(tradeResult: TradeResult) =
            Option.when(tradeResult.quantity > 0) {
              printer.hierarchy.leaf(tradeResult.format)
            }

          val detailsOpt = printer.hierarchy.optionalTree(ac.asset, showDetails)(
            printer.hierarchy.optionalTree("[Evento]")(
              Option.when(ac.postEventPosition.quantity != ac.previousPosition.quantity) {
                printer.hierarchy.leaf(s"Posição: ${ac.previousPosition.signedFormat} -> ${ac.postEventPosition.signedFormat}")
              },
              tradeResultDetailsOpt(ac.eventTradeResult),
            ),
            printer.hierarchy.optionalTree("[Day-Trade]")(
              tradeResultDetailsOpt(ac.dayTradeResult),
            ),
            Option.when(ac.operationsTradeResult.quantity > 0) {
              printer.hierarchy.tree("[Operação Comum]")(
                printer.hierarchy.leaf(
                  formatPositionChange(
                    from = ac.postEventPosition,
                    change = ac.nonDayTradeOperationsAmount.withQuantity(ac.operationsTradeResult.quantity),
                    to = ac.postOperationsTradePosition,
                  )
                ),
                printer.hierarchy.leaf(ac.operationsTradeResult.format),
              )
            },
            Option.when(ac.resultingPosition.quantity != ac.postOperationsTradePosition.quantity) {
              printer.hierarchy.tree("[Carteira]")(
                printer.hierarchy.leaf(
                  formatPositionChange(
                    from = ac.postOperationsTradePosition,
                    change = ac.nonTradeOperationsAmount,
                    to = ac.resultingPosition,
                  )
                )
              )
            }
          )
          for (detailsNode <- detailsOpt) {
            printer.hierarchy.print(detailsNode)
          }

          typeResolver(ac.asset).taxation match {
            case Taxation.ExemptableSwingTrade =>
              TradesResults(
                dayTrade = ac.dayTradeResult,
                exemptableSwingTrade = ac.swingTradeResult,
                totalExemptableSwingTradeSalesValueWithoutCost = ac.swingTradeResult.totalSaleValue,
                nonExemptableSwingTrade = TradeResult.Zero,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.NonExemptableSwingTrade =>
              TradesResults(
                dayTrade = ac.dayTradeResult,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableSwingTrade = ac.swingTradeResult,
                fiisTrade = TradeResult.Zero,
              )
            case Taxation.FII =>
              TradesResults(
                dayTrade = ac.dayTradeResult,
                exemptableSwingTrade = TradeResult.Zero,
                totalExemptableSwingTradeSalesValueWithoutCost = 0.0,
                nonExemptableSwingTrade = TradeResult.Zero,
                fiisTrade = ac.swingTradeResult,
              )
            case Taxation.NonVariableIncome => TradesResults.Zero
          }
        }
        .reduce(_ + _)
    }

  private def formatPositionChange(from: AmountWithCost, change: AmountWithCost, to: AmountWithCost): String =
    s"Posição: ${formatPositionChangeRaw(from, change, to)}"
}
