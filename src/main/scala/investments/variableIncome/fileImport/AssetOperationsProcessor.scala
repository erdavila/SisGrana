package sisgrana
package investments.variableIncome.fileImport

import investments.variableIncome.model.ctx.{localDateEncoder => _, _}
import investments.variableIncome.model.{Amount, AmountWithCost, AssetChange, LocalDateSupport, ctx}
import java.time.LocalDate

class AssetOperationsProcessor(
  stockbroker: String,
  date: LocalDate,
  includeCost: (Amount, Operation) => AmountWithCost
) extends LocalDateSupport {
  def process(asset: String, operationsAmounts: OperationsAmounts): Unit = {
    val purchaseAmountWithCost = includeCost(operationsAmounts.purchase, Operation.Purchase)
    val saleAmountWithCost = includeCost(operationsAmounts.sale, Operation.Sale)

    val currentResultingAmountWithCost =
      getCurrentResultingAmountWithCost(asset, stockbroker, date)
        .getOrElse(AmountWithCost.Zero)

    val updatedResultingAmountWithCost = makeUpdatedResultingAmountWithCost(
      currentResultingAmountWithCost,
      purchaseAmountWithCost,
      saleAmountWithCost,
    )

    save(asset, purchaseAmountWithCost, saleAmountWithCost, updatedResultingAmountWithCost)
  }

  private def getCurrentResultingAmountWithCost(asset: String, stockbroker: String, date: LocalDate): Option[AmountWithCost] = {
    val latestDateResult = ctx.run(
      ctx.query[AssetChange]
        .filter(ac => ac.asset == lift(asset) && ac.stockbroker == lift(stockbroker))
        .map(_.date)
        .max
    )

    latestDateResult.map { latestDate =>
      try {
        if (latestDate.isAfter(date)) {
          throw new Exception(s"Encontrado registro mais recente referente a $latestDate")
        } else {
          val result = ctx.run(
            ctx.query[AssetChange]
              .filter(ac =>
                ac.asset == lift(asset)
                  && ac.stockbroker == lift(stockbroker)
                  && ac.date == lift(latestDate)
              )
          )
          assert(result.length == 1)
          val assetChange = result.head
          if (latestDate == date && (assetChange.purchaseQuantity > 0 || assetChange.saleQuantity > 0)) {
            throw new Exception(s"Encontrado registro com operações na mesma data: $assetChange")
          }

          assetChange.resultingAmountWithCost
        }
      } catch {
        case e: Exception =>
          throw new Exception(s"Exception while getting current resulting values for $asset $stockbroker $date", e)
      }
    }
  }

  private def makeUpdatedResultingAmountWithCost(
    currentResultingAmount: AmountWithCost,
    purchaseAmount: AmountWithCost,
    saleAmount: AmountWithCost,
  ): AmountWithCost = {
    val quantityDelta = purchaseAmount.quantity - saleAmount.quantity
    if (quantityDelta > 0) {
      val nonDayTradePurchaseAmountWithCost = purchaseAmount.modifyQuantity(_ => quantityDelta)
      currentResultingAmount + nonDayTradePurchaseAmountWithCost
    } else {
      currentResultingAmount.modifyQuantity(_ + quantityDelta)
    }
  }

  private def save(asset: String, purchase: AmountWithCost, sale: AmountWithCost, resulting: AmountWithCost): Unit = {
    ctx.run {
      val liftedPurchase = lift(purchase)
      val liftedSale = lift(sale)
      val liftedResulting = lift(resulting)

      ctx.query[AssetChange]
        .insert(
          AssetChange(
            asset = lift(asset),
            stockbroker = lift(stockbroker),
            date = lift(date),
            byEvent = false,
            purchaseQuantity = liftedPurchase.quantity,
            purchaseTotalValue = liftedPurchase.totalValue,
            purchaseCostTotal = liftedPurchase.totalCost,
            saleQuantity = liftedSale.quantity,
            saleTotalValue = liftedSale.totalValue,
            saleCostTotal = liftedSale.totalCost,
            resultingQuantity = liftedResulting.quantity,
            resultingTotalValue = liftedResulting.totalValue,
            resultingCostTotalValue = liftedResulting.totalCost,
          )
        )
        .onConflictUpdate(_.asset, _.stockbroker, _.date)(
          (t, e) => t.purchaseQuantity -> e.purchaseQuantity,
          (t, e) => t.purchaseTotalValue -> e.purchaseTotalValue,
          (t, e) => t.purchaseCostTotal -> e.purchaseCostTotal,
          (t, e) => t.saleQuantity -> e.saleQuantity,
          (t, e) => t.saleTotalValue -> e.saleTotalValue,
          (t, e) => t.saleCostTotal -> e.saleCostTotal,
          (t, e) => t.resultingQuantity -> e.resultingQuantity,
          (t, e) => t.resultingTotalValue -> e.resultingTotalValue,
          (t, e) => t.resultingCostTotalValue -> e.resultingCostTotalValue,
        )
    }
  }
}
