package sisgrana
package investments.variableIncome.fileImport

import com.softwaremill.quicklens._
import investments.variableIncome.model.{CustomEncoding, ctx}
import java.io.File
import java.time.LocalDate

object Main extends CustomEncoding {

  private lazy val nameNormalizer = NameNormalizer.get()

  def main(args: Array[String]): Unit = {
    val brokerageNoteFileNames = args
      .flatMap { arg =>
        val file = new File(arg)
        file.getName match {
          case EventsFileName.Regex(dateString) =>
            val date = LocalDate.parse(dateString)
            Some(EventsFileName(date, file))
          case BrokerageNoteFileName.Regex(dateString, stockbroker) =>
            val date = LocalDate.parse(dateString)
            Some(BrokerageNoteFileName(date, stockbroker, file))
          case _ =>
            Console.err.println(s"Ignorando $file")
            None
        }
      }
      .sortBy {
        case EventsFileName(date, _) => (date, 1)
        case BrokerageNoteFileName(date, _, _) => (date, 2)
      }

    ctx.transaction {
      for (f <- brokerageNoteFileNames) {
        println(s"Importando ${f.file}")
        f match {
          case fn@BrokerageNoteFileName(_, _, _) => processBrokerageNoteFile(fn)
          case fn@EventsFileName(_, _) => processEventsFile(fn)
        }
      }
    }
  }

  private def processBrokerageNoteFile(fileName: BrokerageNoteFileName): Unit = {
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

    val assetProcessor = new AssetOperationsProcessor(fileName.stockbroker, fileName.date, includeCost)

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

  private def processEventsFile(fileName: EventsFileName): Unit = {
    val events = Events.fromFile(fileName.file)
    for (event <- events) {
      val processor = new EventProcessor(event, fileName.date)
      processor.process()
    }
  }
}
