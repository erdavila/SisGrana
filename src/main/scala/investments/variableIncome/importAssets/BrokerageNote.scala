package sisgrana
package investments.variableIncome.importAssets

import investments.utils.BrNumber
import investments.variableIncome.AssetType
import java.io.File
import java.time.LocalDate
import utils.{DoubleOps, quoted}

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
  def fromFile(date: LocalDate, stockbroker: String, nameNormalizer: NameNormalizer)(file: File): BrokerageNote =
    try {
      val linesValues = SSV.readFile(file)

      val (negotiationsLinesValues, remaining1) = linesValues.span(_.nonEmpty)
      val (costsLinesValues, remaining2) = remaining1.drop(1).span(_.nonEmpty)
      val totalString = remaining2.drop(1) match {
        case Seq(totalLineValues, remaining3@_*) =>
          def singleValue(lineValues: Seq[String], name: String): String =
            lineValues match {
              case Seq(value) => value
              case _ => throw new Exception(s"Invalid values for $name: $lineValues")
            }
          val totalString = singleValue(totalLineValues, "total")

          if (remaining3.isEmpty) totalString
          else throw new Exception(s"Exceeding data: $remaining3")
        case _ => throw new Exception("Missing data")
      }

      val negotiations = negotiationsLinesValues.map(Negotiation.parseLineValues(nameNormalizer))
      val costs = costsLinesValues.map(Cost.parseLineValues)
      val total = BrNumber.parse(totalString)

      val note = BrokerageNote(stockbroker, date, negotiations.toList, costs.toList, total)
      note.checkTotalValue()
      note
    } catch {
      case t: Throwable => throw new Exception(s"An exception was thrown while reading file $file", t)
    }
}
