package sisgrana
package investments.commands.incomeRate

import cats.data.State
import java.time.LocalDate
import java.time.format.DateTimeParseException
import scala.annotation.tailrec
import utils.quoted

case class AssetFilter(
  asset: Option[String] = None,
  stockbroker: Option[String] = None,
  portfolio: Option[String] = None,
  minDate: Option[LocalDate] = None,
  maxDate: Option[LocalDate] = None,
) {
  require(!asset.contains(""))
  require(!stockbroker.contains(""))
  for {
    minDate <- minDate
    maxDate <- maxDate
  } {
    import utils.dateOrdering._
    require(minDate <= maxDate)
  }
}

object AssetFilter {
  private val StockbrokerDelimiter = ':'
  private val PortfolioDelimiter = '@'
  private val MinDateDelimiter = '>'
  private val MaxDateDelimiter = '<'

  def parse(string: String): AssetFilter = {
    val consumer = for {
      asset <- consumePart
      stockbroker <- consumePartStartingWith(StockbrokerDelimiter)
      portfolio <- consumePartStartingWith(PortfolioDelimiter)
      minDate <- consumeDatePartStartingWith(MinDateDelimiter)
      maxDate <- consumeDatePartStartingWith(MaxDateDelimiter)
    } yield AssetFilter(asset, stockbroker, portfolio, minDate, maxDate)

    try {
      val (remaining, filter) = consumer.run(string).value
      if (remaining.nonEmpty) throw new IllegalArgumentException(s"Caracteres em excesso: ${quoted(remaining)}")
      filter
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Filtro inválido: ${quoted(string)}\n${e.getMessage}", e)
    }
  }

  private def consumeDatePartStartingWith(char: Char): State[String, Option[LocalDate]] =
    for {
      partOpt <- consumePartStartingWith(char)
      dateOpt = partOpt.map { part =>
        try {
          LocalDate.parse(part)
        } catch {
          case e: DateTimeParseException => throw new IllegalArgumentException(s"Data inválida: ${quoted(part)}", e)
        }
      }
    } yield dateOpt

  private def consumePartStartingWith(char: Char): State[String, Option[String]] =
    State { string =>
      if (string.headOption.contains(char)) {
        consumePart.run(string.tail).value
      } else {
        (string, None)
      }
    }

  private[incomeRate] def consumePart: State[String, Option[String]] =
    State { string =>
      val Delimiters = Set(StockbrokerDelimiter, PortfolioDelimiter, MinDateDelimiter, MaxDateDelimiter)

      val consumed = new StringBuilder

      @tailrec
      def loop(remaining: String): String =
        if (remaining.isEmpty) {
          ""
        } else {
          val char = remaining.head
          if (Delimiters.contains(char)) {
            remaining
          } else if (char == '\\') {
            val rest = remaining.tail
            require(rest.nonEmpty, s"String incompleto: ${quoted(string)}")
            consumed.append(rest.head)
            loop(rest.tail)
          } else {
            consumed.append(char)
            loop(remaining.tail)
          }
        }

      val remaining = loop(string)

      val consumedOpt = Option.when(consumed.nonEmpty)(consumed.result())
      (remaining, consumedOpt)
    }
}
