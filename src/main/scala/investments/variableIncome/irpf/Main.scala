package sisgrana
package investments.variableIncome.irpf

import investments.utils.BrNumber
import investments.variableIncome.model._
import investments.variableIncome.model.ctx._
import investments.variableIncome.{AssetType, Taxation, assetsAtDate}
import java.time.{LocalDate, Month, Year, YearMonth}
import utils.{IndentedPrinter, oppositeSigns, sameNonZeroSigns}

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

  private def formatPositionConversion(posBefore: Amount, posAfter: Amount, withLabel: Boolean = true): String = {
    val formatted = s"${posBefore.signedFormatParens0} -> ${posAfter.signedFormatParens0}"
    s"${positionLabel(withLabel)}$formatted"
  }

  private[irpf] def formatPositionChange(posBefore: Amount, posAfter: Amount, withLabel: Boolean = true): String = {
    val formatted = {
      def formattedChangeInOppositeDirection = {
        val signal = if (posBefore.signedQuantity < 0) '+' else '-'
        val quantityDelta = math.abs(posBefore.signedQuantity - posAfter.signedQuantity)
        s"${posBefore.signedFormatParens0} $signal $quantityDelta unidades = ${posAfter.signedFormatParens0}"
      }

      if (sameNonZeroSigns(posBefore.signedQuantity, posAfter.signedQuantity)) {
        if (math.abs(posBefore.signedQuantity) < math.abs(posAfter.signedQuantity)) {
          val signal = if (posBefore.signedQuantity > 0) '+' else '-'
          val delta = Amount.fromSignedQuantityAndTotals(
            posAfter.signedQuantity - posBefore.signedQuantity,
            posAfter.signedTotalValue - posBefore.signedTotalValue,
            posAfter.totalCost - posBefore.totalCost,
          )
          s"${posBefore.signedFormatParens0} $signal ${delta.formatParens0} = ${posAfter.signedFormatParens0}"
        } else if (math.abs(posBefore.signedQuantity) > math.abs(posAfter.signedQuantity)) {
          formattedChangeInOppositeDirection
        } else {
          formatPositionConversion(posBefore, posAfter, withLabel = false)
        }
      } else if (oppositeSigns(posBefore.signedQuantity, posAfter.signedQuantity)) {
        formattedChangeInOppositeDirection
      } else {
        formatPositionConversion(posBefore, posAfter, withLabel = false)
      }
    }

    s"${positionLabel(withLabel)}$formatted"
  }

  private def positionLabel(withLabel: Boolean) = if (withLabel) "Posição: " else ""

  private def formatTradeResult(tradeResult: TradeResult) =
    s"Resultado: ${tradeResult.saleAmount.formatParens0} - ${tradeResult.purchaseAmount.formatParens0} = ${tradeResult.totalNetValue.format}"

  implicit private class IterableOps[A](private val iterable: Seq[A]) extends AnyVal {
    def groupAndSortBy[K: Ordering](f: A => K): Seq[(K, Seq[A])] =
      iterable.groupBy(f).toIndexedSeq.sortBy(_._1)
  }

  implicit private class AmountOps(private val amount: Amount) extends AnyVal {
    def formatParens0: String =
      formatValues(signed = false)

    def signedFormatParens0: String =
      formatValues(signed = true)

    private def formatValues(signed: Boolean) =
      if (amount.quantity == 0) {
        "0"
      } else {
        val (qty, total) =
          if (signed) {
            (amount.signedQuantity, amount.signedTotalValueWithCost)
          } else {
            (amount.quantity, amount.totalValueWithCost)
          }
        s"($qty x ${amount.averagePriceWithCost.format} = ${total.format})"
      }
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
          val detailsOpt = printer.hierarchy.optionalTree(ac.asset, showDetails)(
            ac.eventEffect.flatMap {
              case EventEffect.SetPosition(pos) =>
                printer.hierarchy.optionalTree("[Evento]")(
                  Some(formatPositionConversion(ac.previousPosition, pos))
                )
              case EventEffect.AddToPosition(_, _) =>
                printer.hierarchy.optionalTree("[Evento]")(
                  Some(
                    if (oppositeSigns(ac.previousPosition.signedQuantity, ac.postEventPosition.signedQuantity)) {
                      formatPositionChange(ac.previousPosition, Amount.Zero)
                    } else {
                      formatPositionChange(ac.previousPosition, ac.postEventPosition)
                    }
                  ),
                  Option.when(ac.eventTradeResult.quantity > 0) {
                    formatTradeResult(ac.eventTradeResult)
                  },
                  Option.when(oppositeSigns(ac.previousPosition.signedQuantity, ac.postEventPosition.signedQuantity)) {
                    formatPositionChange(Amount.Zero, ac.postEventPosition)
                  },
                )
            },
            Option.when(ac.dayTradeResult.quantity > 0) {
              printer.hierarchy.tree("[Day-Trade]")(
                formatTradeResult(ac.dayTradeResult),
              )
            },
            Option.when(ac.postEventPosition.signedQuantity != ac.resultingPosition.signedQuantity) {
              printer.hierarchy.tree("[Carteira]")(
                formatPositionChange(ac.postEventPosition, ac.resultingPosition),
              )
            },
            Option.when(ac.operationsTradeResult.quantity > 0) {
              printer.hierarchy.tree("[Operação Comum]")(
                formatTradeResult(ac.operationsTradeResult),
              )
            },
          )

          for (details <- detailsOpt) {
            printer.hierarchy.print(details)
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
}
