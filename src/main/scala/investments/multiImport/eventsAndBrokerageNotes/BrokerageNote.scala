package sisgrana
package investments.multiImport.eventsAndBrokerageNotes

import investments.AssetType
import java.io.InputStream
import java.time.LocalDate
import utils.Traversing._
import utils.{BrNumber, DoubleOps, SSV, Traverser, quoted}

sealed trait Operation

object Operation {
  case object Purchase extends Operation
  case object Sale extends Operation
}

case class Cost(name: String, value: Double) {
  require(value >= 0.0)
}

object Cost {
  def parseLineValues(lineValues: Seq[String]): Cost =
    SSV.matchValues(lineValues) { case Seq(name, valueString) =>
      val value = BrNumber.parse(valueString)
      Cost(name, value)
    }
}

case class Negotiation(operation: Operation, asset: String, quantity: Int, price: Double, optionAsset: Option[String]) {
  require(quantity > 0)
  require(price >= 0.0)

  lazy val totalValue: Double = quantity * price
}

object Negotiation {
  def parseLineValues(nameNormalizer: NameNormalizer)(lineValues: Seq[String]): Negotiation = {
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
}

case class BrokerageNote(
  stockbroker: String,
  date: LocalDate,
  negotiations: List[Negotiation],
  costs: List[Cost],
  totalValue: Double,
) {

  lazy val totalCosts: Double = costs.map(_.value).sum

  def subjectAssets: Set[String] =
    negotiations.flatMap(n => n.asset +: n.optionAsset.toSeq).toSet

  def checkTotalValue(): Unit = {
    val expectedTotalValue = -totalCosts +
      negotiations
        .map { n =>
          n.operation match {
            case Operation.Purchase => -n.totalValue
            case Operation.Sale => n.totalValue
          }
        }
        .sum

    assert(expectedTotalValue =~= this.totalValue, s"Total value does not match. Check if IRRF was charged")
  }
}

object BrokerageNote {

  def from(date: LocalDate, stockbroker: String, nameNormalizer: NameNormalizer)(inputStream: InputStream): Iterator[BrokerageNote] =
    fromLinesValues(date, stockbroker, nameNormalizer)(SSV.readFrom(inputStream))

  private[eventsAndBrokerageNotes] def fromLinesValues(date: LocalDate, stockbroker: String, nameNormalizer: NameNormalizer)(linesValues: Iterator[SSV.LineValues]): Iterator[BrokerageNote] = {
    val traverser =
      for {
        parsedSome <- getState
        _ <- if (parsedSome) skipBlankLine[Boolean] else nothing[Boolean, SSV.LineValues]

        negotiations <- takeNegotiations(nameNormalizer)
        costs <- takeCosts
        total <- takeTotal

        parsedSome = true
        _ <- setState(parsedSome)
        note = BrokerageNote(stockbroker, date, negotiations.toList, costs.toList, total)
        _ = note.checkTotalValue()
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
      negotiations = linesValues.map(Negotiation.parseLineValues(nameNormalizer))
      _ <- skipBlankLine
    } yield negotiations

  private def takeCosts[S] =
    for {
      linesValues <- Traverser.takeWhile[S, SSV.LineValues](_.nonEmpty)
      costs = linesValues.map(Cost.parseLineValues)
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
}
