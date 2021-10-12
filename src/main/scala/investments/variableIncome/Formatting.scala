package sisgrana
package investments.variableIncome

import investments.utils.BrNumber
import investments.variableIncome.model._
import utils.{IndentedPrinter, oppositeSigns, sameNonZeroSigns}

object Formatting {
  def forAssetPeriod(ap: AssetPeriod, convertedTo: Option[ConvertedTo]): Seq[Option[IndentedPrinter.Node]] =
    Seq(
      ap.eventEffect.flatMap {
        case EventEffect.SetPosition(pos) =>
          val conv = convertedTo
            .filter(_.asset != ap.asset)
            .fold("")(convTo => s" (convertido em ${BrNumber.format(convTo.quantity)} ${convTo.asset})")
          IndentedPrinter.Hierarchy.optionalTree("[Evento]")(
            Some(formatPositionConversion(ap.previousPosition, pos) ++ conv)
          )
        case EventEffect.AddToPosition(_, _) =>
          IndentedPrinter.Hierarchy.optionalTree("[Evento]")(
            Some(
              if (oppositeSigns(ap.previousPosition.signedQuantity, ap.postEventPosition.signedQuantity)) {
                formatPositionChange(ap.previousPosition, Amount.Zero)
              } else {
                formatPositionChange(ap.previousPosition, ap.postEventPosition)
              }
            ),
            Option.when(ap.eventTradeResult.quantity > 0) {
              formatTradeResult(ap.eventTradeResult)
            },
            Option.when(oppositeSigns(ap.previousPosition.signedQuantity, ap.postEventPosition.signedQuantity)) {
              formatPositionChange(Amount.Zero, ap.postEventPosition)
            },
          )
      },
      Option.when(ap.dayTradeResult.quantity > 0) {
        IndentedPrinter.Hierarchy.tree("[Day-Trade]")(
          formatTradeResult(ap.dayTradeResult),
        )
      },
      Option.when(ap.postEventPosition.signedQuantity != ap.resultingPosition.signedQuantity) {
        IndentedPrinter.Hierarchy.tree("[Carteira]")(
          formatPositionChange(ap.postEventPosition, ap.resultingPosition),
        )
      },
      Option.when(ap.operationsTradeResult.quantity > 0) {
        IndentedPrinter.Hierarchy.tree("[Operação Comum]")(
          formatTradeResult(ap.operationsTradeResult),
        )
      },
    )

  private def formatPositionConversion(posBefore: Amount, posAfter: Amount, withLabel: Boolean = true): String = {
    val formatted = s"${posBefore.signedFormatParens0} -> ${posAfter.signedFormatParens0}"
    s"${positionLabel(withLabel)}$formatted"
  }

  private[variableIncome] def formatPositionChange(posBefore: Amount, posAfter: Amount, withLabel: Boolean = true): String = {
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
            posAfter.signedGrossValue - posBefore.signedGrossValue,
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

  private def positionLabel(withLabel: Boolean): String = if (withLabel) "Posição: " else ""

  private def formatTradeResult(tradeResult: TradeResult) =
    s"Resultado: ${tradeResult.saleAmount.formatParens0} - ${tradeResult.purchaseAmount.formatParens0} = ${tradeResult.totalNetValue.format}"

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
            (amount.signedQuantity, amount.signedNetValue)
          } else {
            (amount.quantity, amount.netValue)
          }
        s"($qty x ${amount.averagePriceWithCost.format} = ${total.format})"
      }
  }

  implicit private[variableIncome] class DoubleOps(private val double: Double) extends AnyVal {
    def format: String = BrNumber.formatMoney(double)
  }
}
