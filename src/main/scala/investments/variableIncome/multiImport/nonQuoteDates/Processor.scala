package sisgrana
package investments.variableIncome.multiImport.nonQuoteDates

import investments.variableIncome.files.InputFile
import investments.variableIncome.model.LocalDateSupport._
import investments.variableIncome.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import investments.variableIncome.model.{NonQuoteDate, ctx}
import java.io.InputStream

object Processor {
  def process(inputFiles: Seq[InputFile[NonQuoteDatesFileName.type]]): Unit =
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
