package sisgrana
package investments.variableIncome.importAssets

import com.softwaremill.quicklens._
import investments.irpf.Events
import java.io.File
import scala.annotation.tailrec
import sisgrana.investments.utils.BrNumber

case class Event(from: Event.From, tos: Vector[Event.To]) {
  def formatted: String =
    s"${from.formatted} -> ${tos.map(_.formatted).mkString(" + ")}"
}

object Event {
  case class From(quantity: Int, asset: String) {
    require(quantity > 0)
    def formatted: String = s"$quantity $asset"
  }

  sealed trait AveragePriceDefinition {
    def formatted: String
    def apply(averagePrice: Double): Double
    def apply(averagePrice: Double, averageCost: Double): (Double, Double)
  }

  object AveragePriceDefinition {
    case class Constant(value: Double) extends AveragePriceDefinition {
      override def formatted: String = BrNumber.formatMoney(value)
      override def apply(averagePrice: Double): Double = value
      override def apply(averagePrice: Double, averageCost: Double): (Double, Double) = (value, 0.0)
    }

    case class Multiplier(multiplier: Double) extends AveragePriceDefinition {
      override def formatted: String = BrNumber.format(multiplier) ++ "x"
      override def apply(averagePrice: Double): Double = multiplier * averagePrice
      override def apply(averagePrice: Double, averageCost: Double): (Double, Double) = (apply(averagePrice), apply(averageCost))
    }

    def parse(string: String): AveragePriceDefinition =
      string match {
        case s"$x/$y" => Multiplier(x.toDouble / y.toDouble)
        case s"$n%" => Multiplier(BrNumber.parse(n) / 100.0)
        case s"${n}x" => Multiplier(BrNumber.parse(n))
        case _ => Constant(BrNumber.parse(string))
      }
  }

  case class To(quantity: Int, asset: String, averagePriceDefinition: AveragePriceDefinition) {
    def formatted: String = s"$quantity $asset ${averagePriceDefinition.formatted}"
  }

  def parseLineValues(lineValues: Seq[String]): Event = {
    def makeTo(toQty: String, toAsset: String, toAvgPriceRate: String): To =
      To(toQty.toInt, toAsset, AveragePriceDefinition.parse(toAvgPriceRate))

    @tailrec
    def parseMoreTos(event: Event, elems: Seq[String]): Event =
      elems match {
        case Seq("+", toQty, toAsset, toAvgPriceRate, rest@_*) =>
          val to = makeTo(toQty, toAsset, toAvgPriceRate)
          val newEvent = event.modify(_.tos).using(_ :+ to)
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

object Events {
  type Type = Seq[Event]

  def fromFile(file: File): Events =
    SSV.readFile(file).map(Event.parseLineValues)
}
