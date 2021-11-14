package sisgrana
package investments.model

import investments.model.ctx._
import java.time.LocalDate

trait LocalDateSupport {
  val MaxDate: LocalDate = LocalDate.of(9999, 12, 31)

  implicit val localDateDecoder: Decoder[LocalDate] =
    decoder((index, row) => LocalDate.parse(row.getObject(index).toString))

  implicit val localDateEncoder: Encoder[LocalDate] =
    encoder(java.sql.Types.OTHER, (index, value, row) => row.setObject(index, value.toString, java.sql.Types.OTHER))

  implicit class LocalDateOps(localDate: LocalDate) {
    //noinspection TypeAnnotation
    def <(other: LocalDate) = quote(infix"$localDate < $other".asCondition)

    //noinspection TypeAnnotation
    def >(other: LocalDate) = quote(infix"$localDate > $other".asCondition)

    //noinspection TypeAnnotation
    def <=(other: LocalDate) = quote(infix"$localDate <= $other".asCondition)

    //noinspection TypeAnnotation
    def >=(other: LocalDate) = quote(infix"$localDate >= $other".asCondition)
  }
}

object LocalDateSupport extends LocalDateSupport
