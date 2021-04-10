package sisgrana
package investments.irpf

import investments.irpf.TSV.Elements
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
  def parseTsvElements(elements: Elements): Cost =
    Elements.matching(elements) { case List(name, valueString) =>
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
  def parseElements(names: Names)(elements: Elements): Negotiation =
    Elements.matching(elements) { case List(operationString, asset, qtyString, priceString) =>
      val operation = operationString match {
        case "C" => Operation.Purchase
        case "V" => Operation.Sale
      }
      val qty = qtyString.toInt
      val price = BrNumber.parse(priceString)

      Negotiation(operation, names.normalize(asset), qty, price)
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
  def fromFile(date: LocalDate, stockbroker: String, names: Names)(file: File): BrokerageNote =
    TSV.fromFile(file) { content =>
      val (negotiationsLines, remaining1) = content.span(_.nonEmpty)
      val (costsLines, remaining2) = remaining1.drop(1).span(_.nonEmpty)
      remaining2.drop(1)
      val irrfString = Elements.matching(remaining2.next()) { case List(irrfString) => irrfString }
      val totalString = Elements.matching(remaining2.next()) { case List(totalString) => totalString }

      val negotiations = negotiationsLines.map(Negotiation.parseElements(names)).toList
      val costs = costsLines.map(Cost.parseTsvElements).toList
      val irrf = BrNumber.parse(irrfString)
      val total = BrNumber.parse(totalString)

      BrokerageNote(stockbroker, date, negotiations, costs, irrf, total)
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
      override def apply(averagePrice: Double): Double = ???
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

  def parseTsvElements(elements: Elements): Event = {
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

    Elements.matching(elements) { case List(fromQty, fromAsset, "->", toQty, toAsset, toAvgPriceRate, rest@_*) =>
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
    val events = TSV.fromFile(file) { lines =>
      lines.map(Event.parseTsvElements).toList
    }
    EventsNote(date, events)
  }
}
