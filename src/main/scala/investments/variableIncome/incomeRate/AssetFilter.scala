package sisgrana
package investments.variableIncome.incomeRate

import java.time.LocalDate
import java.time.format.DateTimeParseException
import scala.annotation.tailrec
import utils.quoted

case class AssetFilter(
  asset: Option[String] = None,
  stockbroker: Option[String] = None,
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
  private val MinDateDelimiter = '>'
  private val MaxDateDelimiter = '<'

  def parse(string: String): AssetFilter = {
    val consumer = for {
      asset <- consumePart
      stockbroker <- consumePartStartingWith(StockbrokerDelimiter)
      minDate <- consumeDatePartStartingWith(MinDateDelimiter)
      maxDate <- consumeDatePartStartingWith(MaxDateDelimiter)
    } yield AssetFilter(asset, stockbroker, minDate, maxDate)

    try {
      consumer.consumeAllFrom(string)
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalArgumentException(s"Filtro inválido: ${quoted(string)}\n${e.getMessage}", e)
    }
  }

  private[incomeRate] trait StringConsumer[A] {
    def consumeFrom(string: String): (A, String)

    final def consumeAllFrom(string: String): A = {
      val (a, remaining) = consumeFrom(string)
      require(remaining.isEmpty)
      a
    }

    final def map[B](f: A => B): StringConsumer[B] =
      flatMap { a =>
        val b = f(a)
        StringConsumer.unit(b)
      }

    final def flatMap[B](f: A => StringConsumer[B]): StringConsumer[B] = new StringConsumer[B] {
      override def consumeFrom(string: String): (B, String) = {
        val (a, remaining) = StringConsumer.this.consumeFrom(string)
        val bConsumer = f(a)
        bConsumer.consumeFrom(remaining)
      }
    }
  }

  private object StringConsumer {
    def unit[A](a: A): StringConsumer[A] = new StringConsumer[A] {
      override def consumeFrom(string: String): (A, String) = (a, string)
    }
  }

  private def consumePartStartingWith(char: Char): StringConsumer[Option[String]] = new StringConsumer[Option[String]] {
    override def consumeFrom(string: String): (Option[String], String) =
      if (string.headOption.contains(char)) {
        consumePart.consumeFrom(string.tail)
      } else {
        (None, string)
      }
  }

  private def consumeDatePartStartingWith(char: Char): StringConsumer[Option[LocalDate]] =
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

  private[incomeRate] lazy val consumePart: StringConsumer[Option[String]] = new StringConsumer[Option[String]] {
    override def consumeFrom(string: String): (Option[String], String) = {
      val Delimiters = Set(StockbrokerDelimiter, MinDateDelimiter, MaxDateDelimiter)

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
      (consumedOpt, remaining)
    }
  }
}
