package sisgrana
package investments.commands.multiImport.nonQuoteDates

import investments.fileTypes.nonQuoteDates.{NonQuoteDatesFileName, NonQuoteDatesFileReader}
import investments.files.InputFile
import investments.model.LocalDateSupport._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import investments.model.{NonQuoteDate, ctx}
import java.io.InputStream

object Processor {
  def process(inputFiles: Seq[InputFile[NonQuoteDatesFileName]]): Unit =
    for (inputFile <- inputFiles) {
      println(s"Importando datas sem cotação em ${inputFile.path.stringPath}")
      inputFile.path.read(importNonQuoteDates)
    }

  private def importNonQuoteDates(inputStream: InputStream): Unit =
    ctx.transaction {
      for (nonQuoteDate <- NonQuoteDatesFileReader.readFrom(inputStream)) {
        ctx.run(
          query[NonQuoteDate]
            .insert(lift(nonQuoteDate))
            .onConflictIgnore
        )
      }
    }
}
