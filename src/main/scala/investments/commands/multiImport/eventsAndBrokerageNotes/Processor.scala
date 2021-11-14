package sisgrana
package investments.commands.multiImport.eventsAndBrokerageNotes

import investments.AssetType
import investments.fileTypes.brokerageNotes.{BrokerageNote, BrokerageNoteFileReader, Operation}
import investments.fileTypes.events.{Event, EventsFileReader}
import investments.fileTypes.{BrokerageNoteFileName, EventsFileName, EventsOrBrokerageNoteFileName}
import investments.files.InputFile
import investments.model.LocalDateSupport._
import investments.model._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import java.time.LocalDate
import utils._

object Processor {
  private lazy val nameNormalizer = NameNormalizer.get()

  def process(
    inputFiles: Seq[InputFile[EventsOrBrokerageNoteFileName]],
    filterAssetsFromDate: Option[LocalDate],
    resetAssets: Boolean
  ): Unit = {
    val sortedInputFiles =
      inputFiles
        .pipeWhenMatched(filterAssetsFromDate) {
          case Some(filterDate) => _.filterNot(_.name.date `isBefore` filterDate)
        }
        .groupBy(_.name.date)
        .toSeq
        .sortBy(_._1)

    ctx.transaction {
      for {
        firstInputFile <- inputFiles.headOption
        if resetAssets
      } {
        resetFromDate(firstInputFile.name.date)
      }

      try {
        for ((date, inputFiles) <- sortedInputFiles) {
          processDate(date, inputFiles)
        }
      } catch {
        case e: Throwable =>
          Console.err.println("ABORTANDO. As importações não serão salvas")
          throw e
      }
    }
  }

  private def resetFromDate(resetDate: LocalDate): Unit = {
    import investments.model.LocalDateSupport._

    ctx.run(
      query[AssetPeriod]
        .filter(_.beginDate >= lift(resetDate))
        .delete
    )

    ctx.run(
      query[AssetPeriod]
        .filter(_.endDate >= lift(resetDate))
        .update(
          _.convertedToAsset -> None,
          _.convertedToQuantity -> None,
        )
    )
  }

  private case class LatestPosition(date: LocalDate, position: Amount)

  private def processDate(date: LocalDate, inputFiles: Seq[InputFile[EventsOrBrokerageNoteFileName]]): Unit = {
    println(s"Processando dados de $date")

    val (eventsSeq, brokerageNotesSeq) = inputFiles.partitionMap {
      case InputFile(EventsFileName(_), path) =>
        println(s"  Lendo eventos em ${path.stringPath}")
        Left(path.readFromIterator(EventsFileReader.readFrom))
      case InputFile(BrokerageNoteFileName(date, stockbroker), path) =>
        println(s"  Lendo notas de corretagem em ${path.stringPath}")
        Right(path.readFromIterator(BrokerageNoteFileReader.readFrom(date, stockbroker, nameNormalizer)))
    }
    assert(eventsSeq.lengthIs <= 1)

    val events = eventsSeq.headOption.getOrElse(Seq.empty)
    val brokerageNotes = brokerageNotesSeq.flatten

    val subjectAssets = {
      val eventsAssets =
        for {
          event <- events
          asset <- event.subjectAssets
        } yield asset
      val brokerageNotesAssets =
        for {
          brokerageNote <- brokerageNotes
          asset <- brokerageNote.subjectAssets
        } yield asset

      (eventsAssets ++ brokerageNotesAssets).distinct
    }

    val latestPositions = latestPositionByAsset(subjectAssets)
    for {
      (stockbrokerAsset, latestPosition) <- latestPositions
      if latestPosition.date `isAfter` date
    } throw new Exception(s"Encontrado registro referente a $stockbrokerAsset com data ${latestPosition.date}, que é posterior a $date")

    val previousPositionByAsset =
      for {
        (stockbrokerAsset, latestPosition) <- latestPositions
        if latestPosition.position.quantity != 0
      } yield stockbrokerAsset -> latestPosition.position

    val (assetPeriods, conversions) = processDateChanges(date, previousPositionByAsset, events, brokerageNotes)

    for (ap <- assetPeriods) {
      if (conversions.contains(ap.stockbrokerAsset)) {
        assert(latestPositions.contains(ap.stockbrokerAsset))
      }

      for (latestPosition <- latestPositions.get(ap.stockbrokerAsset)) {
        if (!(latestPosition.date `isBefore` date)) {
          throw new Exception(s"Encontrado registro referente a ${ap.stockbrokerAsset} com data ${latestPosition.date}, que é igual ou posterior a $date")
        }

        val conversionOpt = conversions.get(ap.stockbrokerAsset)
        val convertedToAssetOpt = conversionOpt.map(_.asset)
        val convertedToQuantityOpt = conversionOpt.map(_.quantity)

        ctx.run {
          query[AssetPeriod]
            .filter(_.asset == lift(ap.asset))
            .filter(_.stockbroker == lift(ap.stockbroker))
            .filter(_.beginDate == lift(latestPosition.date))
            .update(
              _.endDate -> lift(date),
              _.convertedToAsset -> lift(convertedToAssetOpt),
              _.convertedToQuantity -> lift(convertedToQuantityOpt),
            )
        }
      }

      ctx.run(query[AssetPeriod].insert(lift(ap)))
    }
  }

  private def latestPositionByAsset(assets: Seq[String]): Map[StockbrokerAsset, LatestPosition] = {
    val result = ctx.run {
      val latestDateByStockbrokerAsset =
        query[AssetPeriod]
          .groupBy(ap => (ap.stockbroker, ap.asset))
          .map { case ((stockbroker, asset), periods) =>
            (stockbroker, asset, periods.map(_.beginDate).max)
          }

      for {
        (stockbroker, asset, dateOpt) <- latestDateByStockbrokerAsset
        ap <- query[AssetPeriod]
        if asset == ap.asset &&
          stockbroker == ap.stockbroker &&
          dateOpt.contains(ap.beginDate) &&
          liftQuery(assets).contains(ap.asset)
      } yield ap
    }

    result
      .map(ap => ap.stockbrokerAsset -> LatestPosition(ap.beginDate, ap.resultingPosition))
      .toMap
  }

  private[eventsAndBrokerageNotes] def processDateChanges(
    date: LocalDate,
    previousPositionByAsset: Map[StockbrokerAsset, Amount],
    events: Seq[Event],
    brokerageNotes: Seq[BrokerageNote],
  ): (Seq[AssetPeriod], Map[StockbrokerAsset, ConvertedTo]) = {
    def initializeAssetPeriod(stockbrokerAsset: StockbrokerAsset) =
      AssetPeriod
        .withZeroes(stockbrokerAsset, date)
        .withPreviousPosition(previousPositionByAsset.getOrElse(stockbrokerAsset, Amount.Zero))

    val eventOutcomeByAsset = processEvents(previousPositionByAsset, events)
    val postEventAssetPeriodsByAsset =
      for ((stockbrokerAsset, eventOutcome) <- eventOutcomeByAsset)
        yield stockbrokerAsset -> initializeAssetPeriod(stockbrokerAsset).withEventEffect(Some(eventOutcome.toEffect))

    def postEventPosition(stockbrokerAsset: StockbrokerAsset): Option[Amount] =
      postEventAssetPeriodsByAsset.get(stockbrokerAsset).map(_.postEventPosition)
        .orElse(previousPositionByAsset.get(stockbrokerAsset))

    val operationsByAsset = brokerageNotes
      .map(processBrokerageNote(postEventPosition, _))
      .reduceOption(_ ++ _) // Assumes that same StockbrokerAsset does not repeat in different BrokerageNotes
      .getOrElse(Map.empty)

    val postOperationsAssetPeriodsByAsset =
      operationsByAsset.foldLeft(postEventAssetPeriodsByAsset) { case (assetPeriods, (stockbrokerAsset, operations)) =>
        assetPeriods.updatedWith(stockbrokerAsset) { postEventAssetPeriodOpt =>
          val assetPeriod = postEventAssetPeriodOpt.getOrElse(initializeAssetPeriod(stockbrokerAsset))

          Some(
            assetPeriod
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

    (postOperationsAssetPeriodsByAsset.values.toSeq, conversions)
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
        val negotiationAverageCost =
          if (totalNegotiationsValue == 0.0) {
            0.0
          } else {
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
