package sisgrana
package investments.fileTypes.fundsMetadata

import investments.files.{DataPath, SSV}
import java.io.{File, FileInputStream}
import utils.use

object FundsMetadataFileReader {
  val FileName = "funds.ssv"

  def read(): Seq[FundMetadata] =
    use(new FileInputStream(new File(DataPath, FileName))) { inputStream =>
      readFrom(inputStream)
    }

  private def readFrom(inputStream: FileInputStream): Seq[FundMetadata] = {
    val fundsMetadata = SSV.readFrom(inputStream)
      .map { lineValues =>
        SSV.matchValues(lineValues) {
          case Seq(cnpj, shortName, longName) => FundMetadata(cnpj, shortName, longName)
        }
      }
      .toSeq

    def checkMultipleRecordsWithSameValue(field: String, fieldAccessor: FundMetadata => String): Unit =
      fundsMetadata
        .groupBy(fieldAccessor)
        .collectFirst { case (value, records) if records.sizeIs > 1 => value }
        .foreach(repeatedValue => throw new Exception(s"""Encontrados múltiplos registros com o mesmo $field "$repeatedValue""""))

    checkMultipleRecordsWithSameValue("CNPJ", _.cnpj)
    checkMultipleRecordsWithSameValue("nome curto", _.shortName)

    fundsMetadata
  }
}
