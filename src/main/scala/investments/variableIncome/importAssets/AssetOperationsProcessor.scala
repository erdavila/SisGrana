package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.AssetOperationsProcessor.calculateSwingTradeResultAndUpdatedPositionAmount
import investments.variableIncome.model._
import investments.variableIncome.model.ctx.{localDateEncoder => _, _}
import java.time.LocalDate

class AssetOperationsProcessor(
  stockbroker: String,
  date: LocalDate,
  includeCostToPurchaseAmount: Amount => PurchaseAmountWithCost,
  includeCostToSaleAmount: Amount => SaleAmountWithCost,
) extends LocalDateSupport {
  def process(asset: String, operationsAmounts: OperationsAmounts): Unit = {
    val purchaseAmountWithCost = includeCostToPurchaseAmount(operationsAmounts.purchase)
    val saleAmountWithCost = includeCostToSaleAmount(operationsAmounts.sale)

    val currentPositionAmountWithCost =
      getCurrentPositionAmountWithCost(asset, stockbroker, date)
        .getOrElse(PurchaseAmountWithCost.Zero)

    val (dayTradeResult, nonDayTradeAmount) = TradeResult.from(purchaseAmountWithCost, saleAmountWithCost)
    val (swingTradeResult, updatedPositionAmountWithCost) = calculateSwingTradeResultAndUpdatedPositionAmount(currentPositionAmountWithCost, nonDayTradeAmount)

    save(asset, purchaseAmountWithCost, saleAmountWithCost, updatedPositionAmountWithCost, dayTradeResult, swingTradeResult)
  }

  private def getCurrentPositionAmountWithCost(asset: String, stockbroker: String, date: LocalDate): Option[AmountWithCost] = {
    val result = ctx.run(
      AssetChange.latestAssetChangesAtDateQuery(MaxDate)
        .filter(ac => ac.asset == lift(asset) && ac.stockbroker == lift(stockbroker))
    )
    assert(result.lengthIs <= 1)
    result.headOption.map { assetChange =>
      try {
        if (assetChange.date.isAfter(date)) {
          throw new Exception(s"Encontrado registro mais recente referente a ${assetChange.date}")
        } else if (assetChange.date == date && (assetChange.purchaseQuantity > 0 || assetChange.saleQuantity > 0)) {
          throw new Exception(s"Encontrado registro com operações na mesma data: $assetChange")
        } else {
          assetChange.position
        }
      } catch {
        case e: Exception =>
          throw new Exception(s"Exception while getting current position values for $asset $stockbroker $date", e)
      }
    }
  }

  private def save(
    asset: String,
    purchase: PurchaseAmountWithCost,
    sale: SaleAmountWithCost,
    position: AmountWithCost,
    dayTrade: TradeResult,
    swingTrade: TradeResult,
  ): Unit = {
    val assetChange = AssetChange.withZeroes(asset, stockbroker, date, byEvent = false)
      .withPurchaseAmount(purchase)
      .withSaleAmount(sale)
      .withPosition(position)
      .withDayTradeResult(dayTrade)
      .withSwingTradeResult(swingTrade)

    ctx.run {
      ctx.query[AssetChange]
        .insert(lift(assetChange))
        .onConflictUpdate(_.asset, _.stockbroker, _.date)(
          (t, e) => t.purchaseQuantity -> e.purchaseQuantity,
          (t, e) => t.purchaseTotalValue -> e.purchaseTotalValue,
          (t, e) => t.purchaseTotalCost -> e.purchaseTotalCost,
          (t, e) => t.saleQuantity -> e.saleQuantity,
          (t, e) => t.saleTotalValue -> e.saleTotalValue,
          (t, e) => t.saleTotalCost -> e.saleTotalCost,
          (t, e) => t.dayTradeResultQuantity -> (t.dayTradeResultQuantity + e.dayTradeResultQuantity),
          (t, e) => t.dayTradeResultTotalGrossValue -> (t.dayTradeResultTotalGrossValue + e.dayTradeResultTotalGrossValue),
          (t, e) => t.dayTradeResultTotalCost -> (t.dayTradeResultTotalCost + e.dayTradeResultTotalCost),
          (t, e) => t.swingTradeResultQuantity -> (t.swingTradeResultQuantity + e.swingTradeResultQuantity),
          (t, e) => t.swingTradeResultTotalGrossValue -> (t.swingTradeResultTotalGrossValue + e.swingTradeResultTotalGrossValue),
          (t, e) => t.swingTradeResultTotalCost -> (t.swingTradeResultTotalCost + e.swingTradeResultTotalCost),
          (t, e) => t.positionQuantity -> e.positionQuantity,
          (t, e) => t.positionTotalValue -> e.positionTotalValue,
          (t, e) => t.positionTotalCost -> e.positionTotalCost,
        )
    }
  }
}

object AssetOperationsProcessor {
  def calculateSwingTradeResultAndUpdatedPositionAmount(
    currentPositionAmount: AmountWithCost,
    nonDayTradeAmount: AmountWithCost,
  ): (TradeResult, AmountWithCost) =
    (currentPositionAmount, nonDayTradeAmount) match {
      case (p1@PurchaseAmountWithCost(_, _, _), p2@PurchaseAmountWithCost(_, _, _)) => (TradeResult.Zero, p1 + p2)
      case (s1@SaleAmountWithCost(_, _, _), s2@SaleAmountWithCost(_, _, _)) => (TradeResult.Zero, s1 + s2)
      case (p@PurchaseAmountWithCost(_, _, _), s@SaleAmountWithCost(_, _, _)) => TradeResult.from(p, s)
      case (s@SaleAmountWithCost(_, _, _), p@PurchaseAmountWithCost(_, _, _)) => TradeResult.from(p, s)
    }
}
