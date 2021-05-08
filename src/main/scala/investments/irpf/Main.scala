package sisgrana
package investments.irpf

import investments.irpf.OwnedAssets.Ops
import java.io.File
import java.time.{LocalDate, YearMonth}
import monocle.syntax.all._

case class FileNameValues(date: LocalDate, name: String, file: File)

case class OperationsAmounts(purchaseAmount: Amount, saleAmount: Amount)

object Main {
  private val DataPath = "data"
  private val EndAssetsFileName = "END - assets.ssv"
  private val NamesFileName = "names.ssv"
  private val TypesFileName = "types.ssv"

  private val SwingTradeExemptableLimit = 20_000.00
  private val SwingTradeTaxRate = 0.15
  private val DayTradeTaxRate = 0.20
  private val FIIsTaxRate = 0.20

  def main(args: Array[String]): Unit = {
    val year = args(0).toInt

    val nameNormalizer = NameNormalizer.fromFile(new File(DataPath, NamesFileName))
    val types = Types.fromFile(new File(DataPath, TypesFileName))

    val previousYearDir = new File(DataPath, (year - 1).toString)
    val currentYearDir = new File(DataPath, year.toString)

    val initialAssetsFile = new File(previousYearDir, EndAssetsFileName)
    val finalAssetsFile = new File(currentYearDir, EndAssetsFileName)

    val fileNames = currentYearDir
      .listFiles()
      .collect { case FileName.select(values) => values }
      .toIndexedSeq

    val main = new Main(types, nameNormalizer)

    val initialOwnedAssets = OwnedAssets.fromFile(initialAssetsFile)
    val finalOwnedAssets = main.process(fileNames)(initialOwnedAssets)
    finalOwnedAssets.writeFile(finalAssetsFile)
  }
}

class Main(types: Types, nameNormalizer: NameNormalizer) {
  import Main.{DayTradeTaxRate, FIIsTaxRate, SwingTradeExemptableLimit, SwingTradeTaxRate}

  private val printer = new IndentedPrinter

  def process(fileNames: IndexedSeq[FileName])(initialOwnedAssets: OwnedAssets): OwnedAssets = {
    val fileNamesByYearMonth = fileNames
      .groupBy(values => YearMonth.from(values.date))
      .toIndexedSeq
      .sortBy(_._1)

    val finalOwnedAssets = fileNamesByYearMonth.foldLeft(initialOwnedAssets) { case (ownedAssets, (yearMonth, fileNames)) =>
      processYearMonth(yearMonth, fileNames)(ownedAssets)
    }

    printer.context("Bens") {
      for (ownedAsset <- finalOwnedAssets.values.toArray.sortBy(ss => (ss.stockbrokerAsset.asset, ss.stockbrokerAsset.stockbroker))) {
        val `type` = types(ownedAsset.stockbrokerAsset.asset)
        val tag =
          if (`type` != Type.Default) s" [${`type`.code}]"
          else ""

        printer.println(s"${ownedAsset.stockbrokerAsset.asset}: ${ownedAsset.amount.formatted} - ${ownedAsset.stockbrokerAsset.stockbroker}$tag")
      }
    }

    finalOwnedAssets
  }

  private def processYearMonth(yearMonth: YearMonth, fileNames: IndexedSeq[FileName])(ownedAssets: OwnedAssets): OwnedAssets =
    printer.context(yearMonth) {
      val sortedFileNames = fileNames.sortBy {
        case EventsFileName(date, _) => (date, "")
        case BrokerageNoteFileName(date, stockbroker, _) => (date, stockbroker)
      }

      val initialYMOutcome = YearMonthOutcome(0.0, SwingTrade.Zero, 0.0, ownedAssets)
      val finalYMOutcome = sortedFileNames.foldLeft(initialYMOutcome) { (ymOutcome, fileName) =>
        fileName match {
          case EventsFileName(date, file) => ymOutcome.focus(_.ownedAssets).modify(processEvents(date, file))
          case BrokerageNoteFileName(date, stockbroker, file) => processBrokerageNote(date, stockbroker, file)(ymOutcome)
        }
      }

      if (finalYMOutcome.dayTradeResult != 0.0) {
        printer.context("Day-Trade") {
          val result = finalYMOutcome.dayTradeResult
          val descr =
            if (result <= 0.0) "prejuízo"
            else "lucro"
          printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
          val tax = result * DayTradeTaxRate
          if (tax > 0.0) {
            printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
          }
        }
      }

      if (finalYMOutcome.swingTrade.totalResult != 0.0) {
        printer.context("Operações Comuns") {
          val exemptableSwingTradeTax =
            if (finalYMOutcome.swingTrade.exemptableResult != 0.0) {
              printer.context("Isentáveis") {
                val totalSales = finalYMOutcome.swingTrade.exemptableTotalSalesWithoutCost
                printer.println(s"Total de vendas: ${BrNumber.formatMoney(totalSales)}")
                val result = finalYMOutcome.swingTrade.exemptableResult
                val isExempt = totalSales < SwingTradeExemptableLimit || result < 0.0
                val descr =
                  if (result <= 0) "prejuízo"
                  else if (isExempt) "lucro isento"
                  else "lucro tributável"
                printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
                if (isExempt) {
                  0.0
                } else {
                  val tax = result * SwingTradeTaxRate
                  printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
                  tax
                }
              }
            } else {
              0.0
            }

          val nonExemptableSwingTradeTax =
            if (finalYMOutcome.swingTrade.nonExemptableResult != 0.0) {
              printer.context("Não isentáveis") {
                val result = finalYMOutcome.swingTrade.nonExemptableResult
                val descr =
                  if (result <= 0.0) "prejuízo"
                  else "lucro"
                printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
                val tax = result * SwingTradeTaxRate
                if (tax > 0.0) {
                  printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
                }
                tax
              }
            } else {
              0.0
            }

          printer.context("Totais") {
            val result = finalYMOutcome.swingTrade.totalResult
            val descr =
              if (result <= 0.0) "prejuízo"
              else "lucro"
            printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
            val tax = exemptableSwingTradeTax + nonExemptableSwingTradeTax
            if (tax > 0.0) {
              printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
            }
          }
        }
      }

      if (finalYMOutcome.fiisResult != 0.0) {
        printer.context("FIIs") {
          val result = finalYMOutcome.fiisResult
          val descr =
            if (result <= 0.0) "prejuízo"
            else "lucro"
          printer.println(s"Resultado: ${BrNumber.formatMoney(result)} ($descr)")
          val tax = result * FIIsTaxRate
          if (tax > 0.0) {
            printer.println(s"Imposto devido: ${BrNumber.formatMoney(tax)}")
          }
        }
      }

      printer.println()

      finalYMOutcome.ownedAssets
    }

  private def processEvents(date: LocalDate, file: File)(ownedAssets: OwnedAssets): OwnedAssets =
    printer.context(s"$date Eventos") {
      val events = Events.fromFile(file)
      events.foldLeft(ownedAssets) { (ownedAssets, event) =>
        processEvent(event)(ownedAssets)
      }
    }

  private def processEvent(event: Event)(ownedAssets: OwnedAssets): OwnedAssets =
    printer.context(event.formatted) {
      val matchingOwnedAssets = ownedAssets.values
        .filter(_.stockbrokerAsset.asset == event.from.asset)

      matchingOwnedAssets.foldLeft(ownedAssets) { (ownedAssets, matchingOwnedAsset) =>
        val times = matchingOwnedAsset.amount.quantity / event.from.quantity
        val removedQuantity = times * event.from.quantity
        val ownedAssetsRemoved = ownedAssets.removeFrom(matchingOwnedAsset.stockbrokerAsset)(removedQuantity)
        printer.println(s"De: ${matchingOwnedAsset.stockbrokerAsset.asset} ${Amount(removedQuantity, matchingOwnedAsset.amount.averagePrice).formatted}")

        val (newOwnedAssets, lines) = event.tos.foldLeft((ownedAssetsRemoved, Vector.empty[String])) { case ((ownedAssets, lines), to) =>
          val stockbrokerAsset = StockbrokerAsset(matchingOwnedAsset.stockbrokerAsset.stockbroker, to.asset)
          val amount = Amount(times * to.quantity, to.averagePriceDefinition(matchingOwnedAsset.amount.averagePrice))
          val newOwnedAssets = ownedAssets.addTo(stockbrokerAsset)(amount)
          (newOwnedAssets, lines :+ s"${to.asset} ${amount.formatted}")
        }

        printer.printHierarchy(
          printer.node("Para:")(lines.map(IndentedPrinter.toNode): _*)
        )

        newOwnedAssets
      }
    }

  private def processBrokerageNote(date: LocalDate, stockbroker: String, file: File)(yearMonthOutcome: YearMonthOutcome): YearMonthOutcome =
    printer.context(s"$date - $stockbroker") {
      val note = BrokerageNote.fromFile(date, stockbroker, nameNormalizer)(file)

      val operationsAmountsPerStockWithoutCosts = aggregateOperationsAmountsPerAsset(note.negotiations)
      val operationsAmountsPerStock = addCosts(note.totalCosts)(operationsAmountsPerStockWithoutCosts)

      operationsAmountsPerStock.toIndexedSeq.sortBy(_._1).foldLeft(yearMonthOutcome) { case (ymOutcome, (asset, opsAmounts)) =>
        val stockbrokerAsset = StockbrokerAsset(note.stockbroker, asset)
        lazy val averageSalePriceWithoutCost = operationsAmountsPerStockWithoutCosts(stockbrokerAsset.asset).saleAmount.averagePrice
        processAsset(stockbrokerAsset, opsAmounts, averageSalePriceWithoutCost)(ymOutcome)
      }
    }

  private def aggregateOperationsAmountsPerAsset(negotiations: Seq[Negotiation]): Map[String, OperationsAmounts] =
    negotiations.foldLeft(Map.empty[String, OperationsAmounts]) { (opsAmountsPerStock, negotiation) =>
      val amount = Amount(negotiation.quantity, negotiation.price)
      opsAmountsPerStock.updatedWith(negotiation.asset) { opsAmountsOpt =>
        val opsAmounts = opsAmountsOpt.getOrElse(OperationsAmounts(Amount.Zero, Amount.Zero))
        val newOpsAmounts =
          negotiation.operation match {
            case Operation.Purchase => opsAmounts.focus(_.purchaseAmount).modify(_.add(amount))
            case Operation.Sale => opsAmounts.focus(_.saleAmount).modify(_.add(amount))
          }
        Some(newOpsAmounts)
      }
    }

  private def addCosts(totalCosts: Double)(opsAmountsPerStock: Map[String, OperationsAmounts]): Map[String, OperationsAmounts] = {
    val totalValues = opsAmountsPerStock.values
      .map { opsAmounts =>
        opsAmounts.purchaseAmount.totalValue + opsAmounts.saleAmount.totalValue
      }
      .sum

    def addOperationCosts(operation: Operation)(amount: Amount): Amount =
      if (amount.quantity == 0) {
        amount
      } else {
        val ratio = amount.totalValue / totalValues
        val proportionalCost = totalCosts * ratio
        val totalValueWithCosts = operation match {
          case Operation.Purchase => amount.totalValue + proportionalCost
          case Operation.Sale => amount.totalValue - proportionalCost
        }
        amount
          .focus(_.averagePrice)
          .replace(totalValueWithCosts / amount.quantity)
      }

    opsAmountsPerStock
      .view.mapValues { opsAmounts =>
      opsAmounts
        .focus(_.purchaseAmount).modify(addOperationCosts(Operation.Purchase))
        .focus(_.saleAmount).modify(addOperationCosts(Operation.Sale))
    }
      .toMap
  }

  private def processAsset(
    stockbrokerAsset: StockbrokerAsset,
    opsAmounts: OperationsAmounts,
    averageSalePriceWithoutCost: => Double
  )(yearMonthOutcome: YearMonthOutcome): YearMonthOutcome = {
    val assetType = types(stockbrokerAsset.asset)
    if (!assetType.variableRate) {
      printer.println(s"${stockbrokerAsset.asset} - Não é renda variável")
      yearMonthOutcome
    } else {
      val amountBefore = yearMonthOutcome.ownedAssets.get(stockbrokerAsset).fold(Amount.Zero)(_.amount)

      val dayTrade = Trade(
        quantity = math.min(opsAmounts.purchaseAmount.quantity, opsAmounts.saleAmount.quantity),
        purchaseAveragePrice = opsAmounts.purchaseAmount.averagePrice,
        saleAveragePrice = opsAmounts.saleAmount.averagePrice,
      )
      val dayTradeLineOpt = Option.when(dayTrade.quantity > 0) {
        printer.node("[Day-Trade]")(s"Resultado: ${dayTrade.formatted}")
      }

      val quantityDelta = opsAmounts.purchaseAmount.quantity - opsAmounts.saleAmount.quantity
      val (swingTrade, amountAfter, swingTradeLinesOpt, nonDayTradePurchaseLinesOpt) =
        if (quantityDelta < 0) {
          val swingTrade = Trade(
            quantity = -quantityDelta,
            purchaseAveragePrice = amountBefore.averagePrice,
            saleAveragePrice = opsAmounts.saleAmount.averagePrice,
          )
          val amountAfter = amountBefore.remove(swingTrade.quantity)
          val swingTradeLinesOpt = Some(
            printer.node("[Operação Comum]")(
              s"Resultado: ${swingTrade.formatted}",
              s"Carteira: ${amountBefore.formattedParens0} - ${swingTrade.quantity} unidades = ${amountAfter.formattedParens0}",
            )
          )
          (swingTrade, amountAfter, swingTradeLinesOpt, None)
        } else if (quantityDelta > 0) {
          val nonDayTradePurchaseAmount = Amount(
            quantity = quantityDelta,
            averagePrice = opsAmounts.purchaseAmount.averagePrice,
          )
          val amountAfter = amountBefore.add(nonDayTradePurchaseAmount)
          val nonDayTradePurchaseLinesOpt = Some(printer.node("[Compra]")(s"Carteira: ${amountBefore.formattedParens0} + ${nonDayTradePurchaseAmount.formattedParens0} = ${amountAfter.formattedParens0}"))
          (Trade.Zero, amountAfter, None, nonDayTradePurchaseLinesOpt)
        } else {
          (Trade.Zero, Amount.Zero, None, None)
        }

      printer.printHierarchy {
        val children = dayTradeLineOpt ++ swingTradeLinesOpt ++ nonDayTradePurchaseLinesOpt
        printer.node(stockbrokerAsset.asset)(children.toSeq: _*)
      }

      val (dayTradeResultDelta, swingTradeDelta, fiisResultDelta) =
        if (assetType.fii) {
          (0.0, SwingTrade.Zero, dayTrade.result + swingTrade.result)
        } else if (assetType.exemptable) {
          val swingTradeDelta = SwingTrade(
            exemptableResult = swingTrade.result,
            exemptableTotalSalesWithoutCost = swingTrade.quantity * averageSalePriceWithoutCost,
            nonExemptableResult = 0.0,
          )
          (dayTrade.result, swingTradeDelta, 0.0)
        } else {
          val swingTradeDelta = SwingTrade(
            exemptableResult = 0.0,
            exemptableTotalSalesWithoutCost = 0.0,
            nonExemptableResult = swingTrade.result,
          )
          (dayTrade.result, swingTradeDelta, 0.0)
        }

      yearMonthOutcome
        .focus(_.dayTradeResult).modify(_ + dayTradeResultDelta)
        .focus(_.swingTrade).modify(_ + swingTradeDelta)
        .focus(_.fiisResult).modify(_ + fiisResultDelta)
        .focus(_.ownedAssets).modify { ownedAssets =>
          if (amountAfter.quantity == 0) ownedAssets.removed(stockbrokerAsset)
          else ownedAssets.updated(stockbrokerAsset, OwnedAsset(stockbrokerAsset, amountAfter))
        }
    }
  }

  private implicit class AmountOps(amount: Amount) {
    def formattedParens0: String =
      if (amount.quantity > 0) s"($formatted)"
      else BrNumber.formatMoney(0.0)

    def formatted: String =
      s"${amount.quantity} x ${BrNumber.formatMoney(amount.averagePrice)} = ${BrNumber.formatMoney(amount.totalValue)}"
  }

  private implicit class TradeOps(trade: Trade) {
    def formatted: String = s"${trade.saleAmount.formattedParens0} - ${trade.purchaseAmount.formattedParens0} = ${BrNumber.formatMoney(trade.result)}"
  }
}
