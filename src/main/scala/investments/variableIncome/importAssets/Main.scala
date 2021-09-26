package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.AssetType
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

  private case class LatestPosition(date: LocalDate, position: Amount)

  private def processDate(date: LocalDate, importedFileNames: Seq[ImportedFileName]): Unit = {
    println(s"Processando dados de $date")

    val (eventsSeq, brokerageNotesSeq) = importedFileNames.partitionMap {
      case ImportedFileName(_, "EVENTS", file) => Left(Event.fromFile(file))
      case ImportedFileName(_, stockbroker, file) => Right(BrokerageNote.fromFile(date, stockbroker, nameNormalizer)(file))
    }
    assert(eventsSeq.lengthIs <= 1)
    val events = eventsSeq.headOption.getOrElse(Seq.empty)
    val brokerageNotes = brokerageNotesSeq.flatten

    val subjectAssets = (
      for {
        event <- events
        asset <- event.subjectAssets
      } yield asset
    ) ++ (
      for {
        brokerageNote <- brokerageNotes
        asset <- brokerageNote.subjectAssets
      } yield asset
    )

    val latestPositions = latestPositionByAsset(subjectAssets.distinct)
    for {
      (stockbrokerAsset, latestPosition) <- latestPositions
      if latestPosition.date `isAfter` date
    } throw new Exception(s"Encontrado registro referente a $stockbrokerAsset com data ${latestPosition.date}, que é posterior a $date")

    val previousPositionByAsset =
      for {
        (stockbrokerAsset, latestPosition) <- latestPositions
        if latestPosition.position.quantity != 0
      } yield stockbrokerAsset -> latestPosition.position

    val (assetChanges, conversions) = processDateChanges(date, previousPositionByAsset, events, brokerageNotes)

    for (ac <- assetChanges) {
      if (conversions.contains(ac.stockbrokerAsset)) {
        assert(latestPositions.contains(ac.stockbrokerAsset))
      }

      for (latestPosition <- latestPositions.get(ac.stockbrokerAsset)) {
        if (!(latestPosition.date `isBefore` date)) {
          throw new Exception(s"Encontrado registro referente a ${ac.stockbrokerAsset} com data ${latestPosition.date}, que é igual ou posterior a $date")
        }

        val conversionOpt = conversions.get(ac.stockbrokerAsset)
        val convertedToAssetOpt = conversionOpt.map(_.asset)
        val convertedToQuantityOpt = conversionOpt.map(_.quantity)

        ctx.run(
          query[AssetChange]
            .filter(_.asset == lift(ac.asset))
            .filter(_.stockbroker == lift(ac.stockbroker))
            .filter(_.date == lift(latestPosition.date))
            .update(
              _.endDate -> lift(date),
              _.convertedToAsset -> lift(convertedToAssetOpt),
              _.convertedToQuantity -> lift(convertedToQuantityOpt),
            )
        )
      }

      ctx.run(query[AssetChange].insert(lift(ac)))
    }
  }

  private def latestPositionByAsset(assets: Seq[String]): Map[StockbrokerAsset, LatestPosition] = {
    val result = ctx.run {
      val latestDateByStockbrokerAsset =
        query[AssetChange]
          .groupBy(ac => (ac.stockbroker, ac.asset))
          .map { case ((stockbroker, asset), changes) =>
            (stockbroker, asset, changes.map(_.date).max)
          }

      for {
        (stockbroker, asset, dateOpt) <- latestDateByStockbrokerAsset
        ac <- query[AssetChange]
        if asset == ac.asset &&
          stockbroker == ac.stockbroker &&
          dateOpt.contains(ac.date) &&
          liftQuery(assets).contains(ac.asset)
      } yield ac
    }

    result
      .map(ac => ac.stockbrokerAsset -> LatestPosition(ac.date, ac.resultingPosition))
      .toMap
  }

  private[importAssets] def processDateChanges(
    date: LocalDate,
    previousPositionByAsset: Map[StockbrokerAsset, Amount],
    events: Seq[Event],
    brokerageNotes: Seq[BrokerageNote],
  ): (Seq[AssetChange], Map[StockbrokerAsset, ConvertedTo]) = {
    def initializeAssetChange(stockbrokerAsset: StockbrokerAsset) =
      AssetChange
        .withZeroes(stockbrokerAsset, date)
        .withPreviousPosition(previousPositionByAsset.getOrElse(stockbrokerAsset, Amount.Zero))

    val eventOutcomeByAsset = processEvents(previousPositionByAsset, events)
    val postEventAssetChangesByAsset =
      for ((stockbrokerAsset, eventOutcome) <- eventOutcomeByAsset)
        yield stockbrokerAsset -> initializeAssetChange(stockbrokerAsset).withEventEffect(Some(eventOutcome.toEffect))

    def postEventPosition(stockbrokerAsset: StockbrokerAsset): Option[Amount] =
      postEventAssetChangesByAsset.get(stockbrokerAsset).map(_.postEventPosition)
        .orElse(previousPositionByAsset.get(stockbrokerAsset))
    val operationsByAsset = brokerageNotes
      .map(processBrokerageNote(postEventPosition, _))
      .reduceOption(_ ++ _) // Assumes that same StockbrokerAsset does not repeat in different BrokerageNotes
      .getOrElse(Map.empty)

    val postOperationsAssetChangesByAsset =
      operationsByAsset.foldLeft(postEventAssetChangesByAsset) { case (assetChanges, (stockbrokerAsset, operations)) =>
        assetChanges.updatedWith(stockbrokerAsset) { postEventAssetChangeOpt =>
          val assetChange = postEventAssetChangeOpt.getOrElse(initializeAssetChange(stockbrokerAsset))

          Some(
            assetChange
              .withPurchaseAmount(operations.purchase)
              .withSaleAmount(operations.sale)
              .withExercisedQuantity(operations.exercisedQuantity)
          )
        }
      }

    val conversions = eventOutcomeByAsset.collect {
      case (stockbrokerAsset, EventOutcome.SetPosition(_, convertedToAsset, convertedToQuantity)) =>
        stockbrokerAsset -> ConvertedTo(convertedToAsset, convertedToQuantity)
    }

    (postOperationsAssetChangesByAsset.values.toSeq, conversions)
  }

  private def processEvents(
    positionByAsset: Map[StockbrokerAsset, Amount],
    events: Seq[Event],
  ): Map[StockbrokerAsset, EventOutcome] =
    events
      .map { event =>
        val processor = new EventProcessor(event)
        processor.process(positionByAsset)
      }
      .reduceOption(EventProcessor.mergeOutcomeByAsset)
      .getOrElse(Map.empty)

  private def processBrokerageNote(
    positionByAsset: StockbrokerAsset => Option[Amount],
    brokerageNote: BrokerageNote,
  ): Map[StockbrokerAsset, AggregatedNegotiations] = {
    val totalNegotiationsValue = brokerageNote.negotiations.map(_.totalValue).sum
    val totalCosts = brokerageNote.totalCosts

    val assetAggNegsList = brokerageNote.negotiations
      .flatMap { negotiation =>
        val negotiationAverageCost = {
          val valuePercentage = negotiation.totalValue / totalNegotiationsValue
          val cost = valuePercentage * totalCosts
          cost / negotiation.quantity
        }

        val (optionSignedAvgPrice, optionAvgCost, optionAggNegsOpt) = negotiation.optionAsset match {
          case Some(optionAsset) =>
            val optionStockbrokerAsset = StockbrokerAsset(brokerageNote.stockbroker, optionAsset)
            val optionPosition = positionByAsset(optionStockbrokerAsset).getOrElse(Amount.Zero)

            val requiredOptionQuantitySign = (AssetType.Option.typeOf(optionAsset), negotiation.operation) match {
              case (AssetType.Option.Type.Call, Operation.Purchase) => +1
              case (AssetType.Option.Type.Call, Operation.Sale) => -1
              case (AssetType.Option.Type.Put, Operation.Purchase) => -1
              case (AssetType.Option.Type.Put, Operation.Sale) => +1
            }
            val exercisedQuantity = requiredOptionQuantitySign * negotiation.quantity

            def notEnoughQuantityMessage = s"Opção $optionAsset tem quantia insuficiente (${optionPosition.signedQuantity}) para ser exercida (necessita $exercisedQuantity)"
            require(optionPosition.quantity >= negotiation.quantity, notEnoughQuantityMessage)
            require(math.signum(optionPosition.signedQuantity) == requiredOptionQuantitySign, notEnoughQuantityMessage)

            val optionAggNegs = AggregatedNegotiations(optionStockbrokerAsset, exercisedQuantity)

            (optionPosition.signedAveragePrice, optionPosition.averageCost, Some(optionAggNegs))
          case None => (0.0, 0.0, None)
        }

        val negotiationAmount =
          (
            negotiation.operation match {
              case Operation.Purchase => (q: Int, p: Double, c: Double) => PurchaseAmount.fromAverages(q, p + optionSignedAvgPrice, c + optionAvgCost)
              case Operation.Sale => (q: Int, p: Double, c: Double) => SaleAmount.fromAverages(q, p - optionSignedAvgPrice, c + optionAvgCost)
            }
          )(negotiation.quantity, negotiation.price, negotiationAverageCost)

        val stockbrokerAsset = StockbrokerAsset(brokerageNote.stockbroker, negotiation.asset)
        val assetAggNegs = AggregatedNegotiations(stockbrokerAsset, negotiationAmount)

        assetAggNegs +: optionAggNegsOpt.toSeq
      }

    assetAggNegsList.groupMapReduce(_.stockbrokerAsset)(identity)(_ + _)
  }
}
