package sisgrana
package investments.variableIncome.model

import investments.variableIncome.model.ctx._
import java.time.LocalDate

trait CustomEncoding {
  implicit val localDateDecoder: Decoder[LocalDate] =
    decoder((index, row) => LocalDate.parse(row.getObject(index).toString))

  implicit val localDateEncoder: Encoder[LocalDate] =
    encoder(java.sql.Types.OTHER, (index, value, row) => row.setObject(index, value.toString, java.sql.Types.OTHER))
}
