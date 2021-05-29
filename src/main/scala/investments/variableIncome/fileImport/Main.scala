package sisgrana
package investments.variableIncome.fileImport

import com.softwaremill.quicklens._
import investments.variableIncome.model.ctx
import java.io.File
import java.time.LocalDate

object Main {

  private lazy val nameNormalizer = NameNormalizer.get()

  def main(args: Array[String]): Unit = {
    val brokerageNoteFileNames = args
      .flatMap { arg =>
        val file = new File(arg)
        file.getName match {
          case BrokerageNoteFileName.Regex(dateString, stockbroker) =>
            val date = LocalDate.parse(dateString)
            Some(BrokerageNoteFileName(date, stockbroker, file))
          case fileName =>
            Console.err.println(s"Ignorando arquivo $fileName")
            None
        }
      }
      .sortBy(_.date)

    ctx.transaction {
      for (f <- brokerageNoteFileNames) {
        processFile(f)
      }
    }
  }

  private def processFile(fileName: BrokerageNoteFileName): Unit = {
    println(s"Importando ${fileName.file.getName}")
    val brokerageNote = BrokerageNote.fromFile(fileName.date, fileName.stockbroker, nameNormalizer)(fileName.file)
    val assetsOpsAmounts = aggregateAssetOperations(brokerageNote)

    val includeCost = {
      val allAssetsOpsTotalValue = assetsOpsAmounts
        .valuesIterator
        .map(oa => oa.purchase.totalValue + oa.sale.totalValue)
        .sum

      val allAssetsOpsTotalCost = brokerageNote.totalCosts

      (opAmount: Amount, operation: Operation) => {
        val opRatio = opAmount.totalValue / allAssetsOpsTotalValue
        val opTotalCost = allAssetsOpsTotalCost * opRatio
        opAmount.withTotalCost(opTotalCost, operation)
      }
    }

    val assetProcessor = new AssetProcessor(fileName.stockbroker, fileName.date, includeCost)

    for ((asset, opsAmounts) <- assetsOpsAmounts) {
      assetProcessor.process(asset, opsAmounts)
    }
  }

  private def aggregateAssetOperations(brokerageNote: BrokerageNote): Map[String, OperationsAmounts] = {
    lazy val zeroOperationsAmounts = OperationsAmounts(Amount.Zero, Amount.Zero)
    brokerageNote.negotiations.foldLeft(Map.empty[String, OperationsAmounts]) { (operationsAmounts, negotiation) =>
      operationsAmounts.updatedWith(negotiation.asset) { existingOpsAmountsOpt =>
        val existingOpsAmounts = existingOpsAmountsOpt.getOrElse(zeroOperationsAmounts)
        val newOpsAmounts = negotiation.operation match {
          case Operation.Purchase => existingOpsAmounts.modify(_.purchase).using(_ + negotiation.amount)
          case Operation.Sale => existingOpsAmounts.modify(_.sale).using(_ + negotiation.amount)
        }
        Some(newOpsAmounts)
      }
    }
  }
}
