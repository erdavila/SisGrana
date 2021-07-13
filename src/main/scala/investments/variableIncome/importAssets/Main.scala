package sisgrana
package investments.variableIncome.importAssets

import com.softwaremill.quicklens._
import investments.irpf.StockbrokerAsset
import investments.variableIncome.model._
import investments.variableIncome.model.ctx._
import java.io.File
import java.time.LocalDate

object Main extends LocalDateSupport {

  private lazy val nameNormalizer = NameNormalizer.get()

  def main(args: Array[String]): Unit = {
    val importedFileNamesByDate = args.toSeq
      .flatMap { arg =>
        val file = new File(arg)
        val ifnOpt = ImportedFileName.from(file)
        if (ifnOpt.isEmpty) {
          Console.err.println(s"Ignorando $file")
        }

        ifnOpt
      }
      .groupBy(_.date)
      .toSeq
      .sortBy(_._1)

    ctx.transaction {
      for ((date, importedFileNames) <- importedFileNamesByDate) {
        processDate(date, importedFileNames)
      }
    }
  }

  private case class LatestPosition(date: LocalDate, position: AmountWithCost)

  private def processDate(date: LocalDate, importedFileNames: Seq[ImportedFileName]): Unit = {
    val (eventsArray, brokerageNotes) = importedFileNames.partitionMap {
      case ImportedFileName(_, "EVENTS", file) => Left(Events.fromFile(file))
      case ImportedFileName(_, stockbroker, file) => Right(BrokerageNote.fromFile(date, stockbroker, nameNormalizer)(file))
    }
    assert(eventsArray.lengthIs <= 1)
    val events = eventsArray.headOption.getOrElse(Seq.empty)

    val subjectAssets = (
      for {
        event <- events
        asset <- event.subjectAssets
      } yield asset
    ) ++ (
      for {
        brokerageNote <- brokerageNotes
        negotiation <- brokerageNote.negotiations
      } yield negotiation.asset
    )

    val latestPositions = latestPositionByAsset(subjectAssets.distinct)
    val previousPositionByAsset =
      for {
        (stockbrokerAsset, latestPosition) <- latestPositions
        if latestPosition.position.quantity != 0
      } yield stockbrokerAsset -> latestPosition.position

    val positionAndSwingTradeResultAfterEventsByAsset = processEvents(previousPositionByAsset, events)
    val assetChangeAfterOperationsByAsset = processBrokerageNotes(previousPositionByAsset ++ positionAndSwingTradeResultAfterEventsByAsset.view.mapValues(_._1), brokerageNotes)

    val assetChanges =
      for {
        stockbrokerAsset <- (positionAndSwingTradeResultAfterEventsByAsset.keySet ++ assetChangeAfterOperationsByAsset.keySet).toSeq
        ac = (positionAndSwingTradeResultAfterEventsByAsset.get(stockbrokerAsset), assetChangeAfterOperationsByAsset.get(stockbrokerAsset)) match {
          case (Some((_, swingTradeResultAfterEvents)), Some(assetChangeAfterOperations)) =>
            assetChangeAfterOperations
              .withSwingTradeResult(assetChangeAfterOperations.swingTradeResult + swingTradeResultAfterEvents)
              .copy(byEvent = true)
          case (Some((positionAfterEvents, swingTradeResultAfterEvents)), None) =>
            AssetChange.withZeroes(stockbrokerAsset, date, byEvent = true)
              .withPosition(positionAfterEvents)
              .withSwingTradeResult(swingTradeResultAfterEvents)
          case (None, Some(assetChangeAfterOperations)) => assetChangeAfterOperations
          case (None, None) => throw new Exception("Can never happen!!!")
        }
      } yield ac

    for (ac <- assetChanges) {
      latestPositions.get(ac.stockbrokerAsset).filterNot(_.date `isBefore` date).foreach(_ =>
        throw new Exception(s"Encontrado registro referente a ${ac.stockbrokerAsset} com data igual ou posterior a $date")
      )
      ctx.run(query[AssetChange].insert(lift(ac)))
    }
  }

  private def latestPositionByAsset(assets: Seq[String]): Map[StockbrokerAsset, LatestPosition] = {
    val result = ctx.run(
      AssetChange.latestAssetChangesAtDateQuery(MaxDate)
        .filter(ac => liftQuery(assets).contains(ac.asset))
    )

    result
      .map(ac => ac.stockbrokerAsset -> LatestPosition(ac.date, ac.position))
      .toMap
  }

  private def processEvents(
    positionByAsset: Map[StockbrokerAsset, AmountWithCost],
    events: Seq[Event],
  ): Map[StockbrokerAsset, (AmountWithCost, TradeResult)] =
    events
      .flatMap { event =>
        val processor = new EventProcessor(event)
        processor.process(positionByAsset)
      }
      .reduceOption { (map1, map2) =>
        val intersection = map1.keySet `intersect` map2.keySet
        if (intersection.nonEmpty) {
          throw new Exception(s"Mais de um evento foram aplicados aos ativo $intersection")
        }
        map1 ++ map2
      }
      .getOrElse(Map.empty)

  private def processBrokerageNotes(
    positionByAsset: Map[StockbrokerAsset, AmountWithCost],
    brokerageNotes: Seq[BrokerageNote],
  ): Map[StockbrokerAsset, AssetChange] =
    brokerageNotes
      .map { processBrokerageNote(positionByAsset, _) }
      .reduceOption(_ ++ _)
      .getOrElse(Map.empty)

  private def processBrokerageNote(
    positionByAsset: Map[StockbrokerAsset, AmountWithCost],
    brokerageNote: BrokerageNote,
  ): Map[StockbrokerAsset, AssetChange] = {
    val assetsOpsAmounts = aggregateAssetOperations(brokerageNote)

    val (includeCostToPurchaseAmount, includeCostToSaleAmount) = {
      val allAssetsOpsTotalValue = assetsOpsAmounts
        .valuesIterator
        .map(oa => oa.purchase.totalValue + oa.sale.totalValue)
        .sum

      val allAssetsOpsTotalCost = brokerageNote.totalCosts

      def includeTo[A <: AmountWithCost](fromTotals: (Int, Double, Double) => A)(opAmount: Amount): A = {
        val opRatio = opAmount.totalValue / allAssetsOpsTotalValue
        val totalCost = allAssetsOpsTotalCost * opRatio
        fromTotals(opAmount.quantity, opAmount.totalValue, totalCost)
      }

      val includeToPurchase = includeTo(PurchaseAmountWithCost.fromTotals)(_)
      val includeToSale = includeTo(SaleAmountWithCost.fromTotals)(_)
      (includeToPurchase, includeToSale)
    }

    val assetProcessor = new AssetOperationsProcessor(
      brokerageNote.stockbroker,
      brokerageNote.date,
      includeCostToPurchaseAmount,
      includeCostToSaleAmount,
    )

    (
      for {
        (asset, opsAmounts) <- assetsOpsAmounts
        stockbrokerAsset = StockbrokerAsset(brokerageNote.stockbroker, asset)
        position = positionByAsset.getOrElse(stockbrokerAsset, PurchaseAmountWithCost.Zero)
        assetChange = assetProcessor.process(asset, opsAmounts, position)
      } yield stockbrokerAsset -> assetChange
    ).toMap
  }

  private def aggregateAssetOperations(brokerageNote: BrokerageNote): Map[String, OperationsAmounts] = {
    lazy val zeroOperationsAmounts = OperationsAmounts(Amount.Zero, Amount.Zero)
    brokerageNote.negotiations.foldLeft(Map.empty[String, OperationsAmounts]) { (operationsAmounts, negotiation) =>
      operationsAmounts.updatedWith(negotiation.asset) { existingOpsAmountsOpt =>
        val existingOpsAmounts = existingOpsAmountsOpt.getOrElse(zeroOperationsAmounts)
        val newOpsAmounts = negotiation.operation match {
          case Operation.Purchase => existingOpsAmounts.modify(_.purchase).using(_ + negotiation.amount)
          case Operation.Sale => existingOpsAmounts.modify(_.sale).using(_ + negotiation.amount)
        }
        Some(newOpsAmounts)
      }
    }
  }
}
