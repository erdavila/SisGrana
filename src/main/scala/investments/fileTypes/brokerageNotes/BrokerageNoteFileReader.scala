package sisgrana
package investments.fileTypes.brokerageNotes

import investments.AssetType
import investments.commands.multiImport.eventsAndBrokerageNotes.NameNormalizer
import investments.files.SSV
import java.io.InputStream
import java.time.LocalDate
import utils.Traversing._
import utils.{BrNumber, DoubleOps, Traverser, quoted}

object BrokerageNoteFileReader {
  def readFrom(date: LocalDate, stockbroker: String, nameNormalizer: NameNormalizer)(inputStream: InputStream): Iterator[BrokerageNote] =
    fromLinesValues(date, stockbroker, nameNormalizer)(SSV.readFrom(inputStream))

  private[brokerageNotes] def fromLinesValues(date: LocalDate, stockbroker: String, nameNormalizer: NameNormalizer)(linesValues: Iterator[SSV.LineValues]): Iterator[BrokerageNote] = {
    val traverser =
      for {
        parsedSome <- getState
        _ <- if (parsedSome) skipBlankLine[Boolean] else nothing[Boolean, SSV.LineValues]

        negotiations <- takeNegotiations(nameNormalizer)
        costs <- takeCosts
        total <- takeTotal
        note = BrokerageNote(stockbroker, date, negotiations.toList, costs.toList, total)
        _ = checkTotalValue(note)

        parsedSome = true
        _ <- setState(parsedSome)
      } yield note

    linesValues.traverse(traverser, false) { (parsedSome, completed) =>
      if (!parsedSome || !completed) throw new Exception("Missing data")
      None
    }
  }

  private def getState[In] = Traverser.getState[Boolean, In]
  private def setState[In](value: Boolean) = Traverser.setState[Boolean, In](value)

  private def skipBlankLine[S] =
    for {
      line <- Traverser.takeNext[S, SSV.LineValues]
      _ = if (line.nonEmpty) throw new Exception("Exceeding data")
    } yield ()

  private def nothing[S, In] = Traverser.const[S, In](())

  private def takeNegotiations[S](nameNormalizer: NameNormalizer) =
    for {
      linesValues <- Traverser.takeWhile[S, SSV.LineValues](_.nonEmpty)
      negotiations = linesValues.map(parseNegotiationLineValues(nameNormalizer))
      _ <- skipBlankLine
    } yield negotiations

  private def takeCosts[S] =
    for {
      linesValues <- Traverser.takeWhile[S, SSV.LineValues](_.nonEmpty)
      costs = linesValues.map(parseCostLineValues)
      _ <- skipBlankLine
    } yield costs

  private def takeTotal[S] =
    for {
      lineValues <- Traverser.takeNext[S, SSV.LineValues]
      value = lineValues match {
        case Seq(value) => value
        case _ => throw new Exception(s"Invalid values for total: $lineValues")
      }
    } yield BrNumber.parse(value)

  private def parseNegotiationLineValues(nameNormalizer: NameNormalizer)(lineValues: Seq[String]): Negotiation = {
    def negotiationFrom(
      operationString: String,
      asset: String,
      quantityString: String,
      priceString: String,
      optionAssetOpt: Option[String],
    ): Negotiation = {
      for {
        optionAsset <- optionAssetOpt
        if !AssetType.Resolver.isOption(optionAsset)
      } throw new Exception(s"Não é uma opção: ${quoted(optionAsset)}")

      Negotiation(
        operation = operationString match {
          case "C" => Operation.Purchase
          case "V" => Operation.Sale
        },
        asset = nameNormalizer.normalize(asset),
        quantity = quantityString.toInt,
        price = BrNumber.parse(priceString),
        optionAsset = optionAssetOpt,
      )
    }

    SSV.matchValues(lineValues) {
      case Seq(operationString, asset, qtyString, priceString) =>
        negotiationFrom(operationString, asset, qtyString, priceString, None)
      case Seq(s"E${operationString}", optionAsset, objectAsset, qtyString, priceString) =>
        negotiationFrom(operationString, objectAsset, qtyString, priceString, Some(optionAsset))
    }
  }

  private def parseCostLineValues(lineValues: Seq[String]): Cost =
    SSV.matchValues(lineValues) { case Seq(name, valueString) =>
      val value = BrNumber.parse(valueString)
      Cost(name, value)
    }

  private def checkTotalValue(brokerageNote: BrokerageNote): Unit = {
    val expectedTotalValue = -brokerageNote.totalCosts +
      brokerageNote.negotiations
        .map { n =>
          n.operation match {
            case Operation.Purchase => -n.totalValue
            case Operation.Sale => n.totalValue
          }
        }
        .sum

    assert(expectedTotalValue =~= brokerageNote.totalValue, s"Total value does not match. Check if IRRF was charged")
  }
}
