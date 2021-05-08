package sisgrana
package investments.irpf

import java.io.File
import java.time.LocalDate

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

case class Negotiation(operation: Operation, asset: String, quantity: Int, price: Double) {
  require(quantity > 0)
  require(price >= 0.0)

  lazy val unsignedValue: Double = quantity * price
}

object Negotiation {
  def parseLineValues(nameNormalizer: NameNormalizer)(lineValues: Seq[String]): Negotiation =
    SSV.matchValues(lineValues) { case Seq(operationString, asset, qtyString, priceString) =>
      val operation = operationString match {
        case "C" => Operation.Purchase
        case "V" => Operation.Sale
      }
      val qty = qtyString.toInt
      val price = BrNumber.parse(priceString)

      Negotiation(operation, nameNormalizer.normalize(asset), qty, price)
    }
}

case class BrokerageNote(
  stockbroker: String, date: LocalDate,
  negotiations: List[Negotiation],
  costs: List[Cost],
  totalValue: Double,
) {
  import BrokerageNote.DoubleOps

  lazy val totalCosts: Double = costs.map(_.value).sum

  def checkTotalValue(): Unit = {
    val expectedTotalValue = -totalCosts +
      negotiations
        .map { n =>
          n.operation match {
            case Operation.Purchase => -n.unsignedValue
            case Operation.Sale => n.unsignedValue
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

  implicit private class DoubleOps(private val x: Double) extends AnyVal {
    def =~=(y: Double): Boolean =
      math.abs(x - y) < 0.01
  }
}
