package sisgrana
package investments.variableIncome.importQuotes

import java.io.FileInputStream

object ImportQuotesMain {
  def main(args: Array[String]): Unit = {
    val filesProcessor = new FilesProcessor
    for (arg <- args) {
      var fis: FileInputStream = null
      filesProcessor.processFile(MultiFile(arg), {
        fis = new FileInputStream(arg)
        fis
      })
      if (fis != null) {
        fis.close()
      }
    }
  }
}
