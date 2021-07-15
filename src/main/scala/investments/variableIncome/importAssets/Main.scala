package sisgrana
package investments.variableIncome.importAssets

import com.softwaremill.quicklens._
import investments.irpf.StockbrokerAsset
import investments.variableIncome.importAssets.AssetOperationsProcessor.OperationsOutcome
import investments.variableIncome.importAssets.EventProcessor.EventOutcome
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
    println(s"Processando dados de $date")

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

    val eventOutcomeByAsset = processEvents(previousPositionByAsset, events)
    val operationsOutcomeByAsset = processBrokerageNotes(previousPositionByAsset ++ eventOutcomeByAsset.view.mapValues(_.position), brokerageNotes)

    val assetChanges =
      for {
        stockbrokerAsset <- (eventOutcomeByAsset.keySet ++ operationsOutcomeByAsset.keySet).toSeq

        initialAssetChange = AssetChange
          .withZeroes(stockbrokerAsset, date, byEvent = false)
          .withPreviousPosition(previousPositionByAsset.getOrElse(stockbrokerAsset, PurchaseAmountWithCost.Zero))

        postEventAssetChange = eventOutcomeByAsset.get(stockbrokerAsset) match {
          case Some(eventOutcome) =>
            initialAssetChange
              .copy(byEvent = true)
              .withEventTradeResult(eventOutcome.tradeResult)
              .withPostEventPosition(eventOutcome.position)
          case None =>
            initialAssetChange
              .withPostEventPosition(initialAssetChange.previousPosition)
        }

        assetChange = operationsOutcomeByAsset.get(stockbrokerAsset) match {
          case Some(operationsOutcome) =>
            postEventAssetChange
              .withPurchaseAmount(operationsOutcome.purchaseAmount)
              .withSaleAmount(operationsOutcome.saleAmount)
              .withDayTradeResult(operationsOutcome.dayTradeResult)
              .withNonDayTradeAmount(operationsOutcome.nonDayTradeAmount)
              .withOperationsTradeResult(operationsOutcome.swingTradeResult)
              .withResultingPosition(operationsOutcome.position)
          case None =>
            postEventAssetChange
              .withResultingPosition(postEventAssetChange.postEventPosition)
        }
      } yield assetChange

    for (ac <- assetChanges) {
      latestPositions.get(ac.stockbrokerAsset).filterNot(_.date `isBefore` date).foreach(existingAC =>
        throw new Exception(s"Encontrado registro referente a ${ac.stockbrokerAsset} com data ${existingAC.date}, que é igual ou posterior a $date")
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
      .map(ac => ac.stockbrokerAsset -> LatestPosition(ac.date, ac.resultingPosition))
      .toMap
  }

  private def processEvents(
    positionByAsset: Map[StockbrokerAsset, AmountWithCost],
    events: Seq[Event],
  ): Map[StockbrokerAsset, EventOutcome] =
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
  ): Map[StockbrokerAsset, OperationsOutcome] =
    brokerageNotes
      .map { processBrokerageNote(positionByAsset, _) }
      .reduceOption(_ ++ _)
      .getOrElse(Map.empty)

  private def processBrokerageNote(
    positionByAsset: Map[StockbrokerAsset, AmountWithCost],
    brokerageNote: BrokerageNote,
  ): Map[StockbrokerAsset, OperationsOutcome] = {
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
      includeCostToPurchaseAmount,
      includeCostToSaleAmount,
    )

    (
      for {
        (asset, opsAmounts) <- assetsOpsAmounts
        stockbrokerAsset = StockbrokerAsset(brokerageNote.stockbroker, asset)
        position = positionByAsset.getOrElse(stockbrokerAsset, PurchaseAmountWithCost.Zero)
        operationsOutcome = assetProcessor.process(opsAmounts, position)
      } yield stockbrokerAsset -> operationsOutcome
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