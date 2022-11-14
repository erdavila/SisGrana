package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement.{FundsMonthStatementFileReader, FundsStatement}
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, YearMonth}
import utils.{Exit, TextAligner}

class StatementRewriter(month: YearMonth) {
  private val SuffixTimestampFormat = DateTimeFormatter.ofPattern("YYYY-MM-dd_HH-mm-ss")

  private val filePath = FundsMonthStatementFileReader.terminalFilePath(month)
  private val file = new File(filePath)

  def read(): FundsStatement = {
    if (!file.exists()) {
      Exit.withErrorMessage { stream =>
        stream.println(s"O arquivo não existe: $filePath")
      }
    }

    FundsMonthStatementFileReader.read(month)
  }

  def rewrite(statement: FundsStatement): Unit = {
    val rowsChunks = MonthStatementChunkMaker.makeChunks(statement)
    renameExistingFile()
    writeFile(rowsChunks)
  }


  private def renameExistingFile(): Unit = {
    val now = LocalDateTime.now()
    val newFilePath = s"${file.getPath}.${SuffixTimestampFormat.format(now)}"
    file.renameTo(new File(newFilePath))

    println(s"Arquivo existente foi renomeado para $newFilePath")
  }

  private def writeFile(rowsChunks: Seq[Seq[TextAligner.Chunk]]): Unit = {
    val writer = new PrintWriter(new FileOutputStream(file))
    for (row <- TextAligner.alignAndRender(rowsChunks)) {
      writer.println(row)
    }
    writer.close()

    println(s"Arquivo regravado: ${file.getPath}")
  }
}
