package sisgrana
package investments.irpf

import java.io.File
import java.time.LocalDate
import monocle.syntax.all._
import scala.annotation.tailrec
import scala.util.matching.Regex

sealed trait InputNote {
  val date: LocalDate
}

object InputNote {
  private val FileNameRegex: Regex = """(\d{4}-\d{2}-\d{2}) - (.+)\.[^.]+""".r

  object FileMatcher {
    def unapply(file: File): Option[(LocalDate, Option[String])] =
      file.getName match {
        case FileNameRegex(dateString, name) =>
          val date = LocalDate.parse(dateString)
          Some((date, Option(name).filter(_ != "EVENTS")))
        case _ => None
      }
  }
}

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
  irrf: Double, totalValue: Double
) extends InputNote {
  import BrokerageNote.DoubleOps

  lazy val totalCosts: Double = costs.map(_.value).sum

  def wasIrrfCharged: Boolean = {
    val totalValue = -totalCosts +
      negotiations
        .map { n =>
          n.operation match {
            case Operation.Purchase => -n.unsignedValue
            case Operation.Sale => n.unsignedValue
          }
        }
        .sum

    if (totalValue =~= this.totalValue) {
      false
    } else if (totalValue + irrf =~= this.totalValue) {
      true
    } else {
      throw new AssertionError(s"Values don't match!\n$totalValue + ${irrf} != ${this.totalValue}")
    }
  }
}

object BrokerageNote {
  def fromFile(date: LocalDate, stockbroker: String, nameNormalizer: NameNormalizer)(file: File): BrokerageNote = {
    val linesValues = SSV.readFile(file)

    val (negotiationsLinesValues, remaining1) = linesValues.span(_.nonEmpty)
    val (costsLinesValues, remaining2) = remaining1.drop(1).span(_.nonEmpty)
    val (irrfString, totalString) = remaining2.drop(1) match {
      case Seq(irrfLineValues, totalLineValues, remaining3@_*) =>
        def singleValue(lineValues: Seq[String], name: String): String =
          lineValues match {
            case Seq(value) => value
            case _ => throw new Exception(s"Invalid values for $name: $lineValues")
          }
        val irrfString = singleValue(irrfLineValues, "IRRF")
        val totalString = singleValue(totalLineValues, "total")

        if (remaining3.isEmpty) (irrfString, totalString)
        else throw new Exception(s"Exceeding data: $remaining3")
      case _ => throw new Exception("Missing data")
    }

    val negotiations = negotiationsLinesValues.map(Negotiation.parseLineValues(nameNormalizer))
    val costs = costsLinesValues.map(Cost.parseLineValues)
    val irrf = BrNumber.parse(irrfString)
    val total = BrNumber.parse(totalString)

    BrokerageNote(stockbroker, date, negotiations.toList, costs.toList, irrf, total)
  }

  implicit private class DoubleOps(private val x: Double) extends AnyVal {
    def =~=(y: Double): Boolean =
      math.abs(x - y) < 0.01
  }
}

case class Event(from: Event.From, tos: Vector[Event.To])

object Event {
  case class From(quantity: Int, asset: String)

  sealed trait AveragePriceDefinition {
    def apply(averagePrice: Double): Double
  }

  object AveragePriceDefinition {
    case class Constant(value: Double) extends AveragePriceDefinition {
      override def apply(averagePrice: Double): Double = value
    }

    case class Multiplier(multiplier: Double) extends AveragePriceDefinition {
      override def apply(averagePrice: Double): Double = multiplier * averagePrice
    }

    def parse(string: String): AveragePriceDefinition =
      string match {
        case s"$x/$y" => Multiplier(x.toDouble / y.toDouble)
        case s"$n%" => Multiplier(BrNumber.parse(n) / 100.0)
        case s"${n}x" => Multiplier(BrNumber.parse(n))
        case _ => Constant(BrNumber.parse(string))
      }
  }

  case class To(quantity: Int, asset: String, averagePriceDefinition: AveragePriceDefinition)

  def parseLineValues(lineValues: Seq[String]): Event = {
    def makeTo(toQty: String, toAsset: String, toAvgPriceRate: String): To =
      To(toQty.toInt, toAsset, AveragePriceDefinition.parse(toAvgPriceRate))

    @tailrec
    def parseMoreTos(event: Event, elems: Seq[String]): Event =
      elems match {
        case Seq("+", toQty, toAsset, toAvgPriceRate, rest@_*) =>
          val to = makeTo(toQty, toAsset, toAvgPriceRate)
          val newEvent = event.focus(_.tos).modify(_ :+ to)
          parseMoreTos(newEvent, rest)
        case Seq() => event
        case _ => throw new MatchError(elems)
      }

    SSV.matchValues(lineValues) { case Seq(fromQty, fromAsset, "->", toQty, toAsset, toAvgPriceRate, rest@_*) =>
      val from = From(fromQty.toInt, fromAsset)
      val to = makeTo(toQty, toAsset, toAvgPriceRate)
      val event = Event(from, Vector(to))
      parseMoreTos(event, rest)
    }
  }
}

case class EventsNote(date: LocalDate, events: List[Event]) extends InputNote

object EventsNote {
  def fromFile(date: LocalDate)(file: File): EventsNote = {
    val events = SSV.readFile(file).map(Event.parseLineValues)
    EventsNote(date, events.toList)
  }
}
