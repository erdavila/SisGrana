package sisgrana
package investments.irpf

import java.io.{File, PrintStream}
import java.time.YearMonth
import monocle.syntax.all._

object VariableIncome {

  private val DataPath = "data"
  private val EndAssetsFileName = "END - assets.ssv"
  private val NamesFileName = "names.ssv"
  private val TypesFileName = "types.ssv"

  private val SwingTradeExemptableLimit = 20_000.00
  private val SwingTradeTaxRate = 0.15
  private val DayTradeTaxRate = 0.20
  private val FIIsTaxRate = 0.20

  def main(args: Array[String]): Unit = {
    val (year, showDebugInfo) = parseOptions(args.toList)

    val nameNormalizer = NameNormalizer.fromFile(new File(DataPath, NamesFileName))
    val types = Types.fromFile(new File(DataPath, TypesFileName))

    val previousYearDir = new File(DataPath, (year - 1).toString)
    val currentYearDir = new File(DataPath, year.toString)

    val initialAssetsFile = new File(previousYearDir, EndAssetsFileName)
    val finalAssetsFile = new File(currentYearDir, EndAssetsFileName)

    val initialOwnedAssets = OwnedAssets.fromFile(initialAssetsFile)
    val inputNotes = currentYearDir
      .listFiles()
      .collect {
        case file@InputNote.FileMatcher(date, Some(stockbroker)) =>
          BrokerageNote.fromFile(date, stockbroker, nameNormalizer)(file)
        case file@InputNote.FileMatcher(date, None) =>
          EventsNote.fromFile(date)(file)
      }

    val debug = if (showDebugInfo) new DebugEnabled else new DebugDisabled
    val vi = new VariableIncome(types, debug)
    val (yearMonthOutcomes, finalOwnedAssets) = vi.processNotes(initialOwnedAssets, inputNotes.toIndexedSeq)
    debug.ifEnabled {
      debug.println("Ended processing")
      debug.println()
    }

    show(yearMonthOutcomes, finalOwnedAssets, types)

    finalOwnedAssets.writeFile(finalAssetsFile)
  }

  private def parseOptions(args: List[String]): (Int, Boolean) = {
    def printUsage(printStream: PrintStream): Unit = {
      printStream.println("Opções:")
      printStream.println("  [-h]")
      printStream.println("  [--debug] ANO")
    }

    def usageError(message: String): Nothing = {
      Console.err.println(message)
      printUsage(Console.err)
      System.exit(1).asInstanceOf[Nothing]
    }

    val (debug, argsRest1) = args match {
      case ("-h" | "--help") :: _ =>
        printUsage(Console.out)
        System.exit(0).asInstanceOf[Nothing]
      case "--debug" :: rest => (true, rest)
      case arg :: _ if arg.startsWith("-") => usageError(s"Opção desconhecida: ${quoted(arg)}")
      case args => (false, args)
    }

    val (year, argsRest2) = argsRest1 match {
      case yearString :: rest =>
        val year = yearString.toIntOption.getOrElse(usageError(s"Ano inválido: ${quoted(yearString)}"))
        (year, rest)
      case Nil => usageError("Faltando especificar o ano")
    }

    argsRest2 match {
      case arg :: _ => usageError(s"Parâmetros em excesso a partir de ${quoted(arg)}")
      case Nil => (year, debug)
    }
  }

  private def show(outcomes: Array[YearMonthOutcome], finalOwnedAssets: OwnedAssets, types: Types): Unit = {
    def formatMoney(value: Double) = s"R$$ ${BrNumber.format(value)}"

    val indentedPrinter = new IndentedPrinter

    for (ymOutcome <- outcomes) {
      indentedPrinter.context(ymOutcome.yearMonth) {
        indentedPrinter.context("Operações Comuns") {
          if (ymOutcome.swingTrade.totalQuantity > 0) {
            val exemptableSwingTradeTax =
              if (ymOutcome.swingTrade.exemptable.quantity > 0) {
                indentedPrinter.context("Isentáveis") {
                  val totalSales = ymOutcome.swingTrade.exemptable.totalSalesWithoutCosts
                  indentedPrinter.println(s"Total de vendas: ${formatMoney(totalSales)}")
                  val result = ymOutcome.swingTrade.exemptable.result
                  val isExempt = totalSales < SwingTradeExemptableLimit || result < 0.0
                  val descr =
                    if (result <= 0) "prejuízo"
                    else if (isExempt) "lucro isento"
                    else "lucro tributável"
                  indentedPrinter.println(s"Resultado: ${formatMoney(result)} ($descr)")
                  if (isExempt) {
                    0.0
                  } else {
                    val tax = result * SwingTradeTaxRate
                    indentedPrinter.println(s"Imposto devido: ${formatMoney(tax)}")
                    tax
                  }
                }
              } else {
                0.0
              }

            val nonExemptableSwingTradeTax =
              if (ymOutcome.swingTrade.nonExemptable.quantity > 0) {
                indentedPrinter.context("Não isentáveis") {
                  val result = ymOutcome.swingTrade.nonExemptable.result
                  val descr =
                    if (result <= 0.0) "prejuízo"
                    else "lucro"
                  indentedPrinter.println(s"Resultado: ${formatMoney(result)} ($descr)")
                  val tax = result * SwingTradeTaxRate
                  if (tax > 0.0) {
                    indentedPrinter.println(s"Imposto devido: ${formatMoney(tax)}")
                  }
                  tax
                }
              } else {
                0.0
              }

            indentedPrinter.context("Totais") {
              val result = ymOutcome.swingTrade.totalResult
              val descr =
                if (result <= 0.0) "prejuízo"
                else "lucro"
              indentedPrinter.println(s"Resultado: ${formatMoney(result)} ($descr)")
              val tax = exemptableSwingTradeTax + nonExemptableSwingTradeTax
              if (tax > 0.0) {
                indentedPrinter.println(s"Imposto devido: ${formatMoney(tax)}")
              }
            }
          } else {
            indentedPrinter.println("Nenhuma venda")
          }
        }

        indentedPrinter.context("Day-Trade") {
          if (ymOutcome.dayTrade.quantity > 0) {
            val result = ymOutcome.dayTrade.result
            val descr =
              if (result <= 0.0) "prejuízo"
              else "lucro"
            indentedPrinter.println(s"Resultado: ${formatMoney(result)} ($descr)")
            val tax = result * DayTradeTaxRate
            if (tax > 0.0) {
              indentedPrinter.println(s"Imposto devido: ${formatMoney(tax)}")
            }
          } else {
            indentedPrinter.println("Nenhuma venda")
          }
        }

        indentedPrinter.context("FIIs") {
          if (ymOutcome.fiisTrade.quantity > 0) {
            val result = ymOutcome.fiisTrade.result
            val descr =
              if (result <= 0.0) "prejuízo"
              else "lucro"
            indentedPrinter.println(s"Resultado: ${formatMoney(result)} ($descr)")
            val tax = result * FIIsTaxRate
            if (tax > 0.0) {
              indentedPrinter.println(s"Imposto devido: ${formatMoney(tax)}")
            }
          } else {
            indentedPrinter.println("Nenhuma venda")
          }
        }
      }

      indentedPrinter.println()
    }

    indentedPrinter.context("Bens") {
      for (oa <- finalOwnedAssets.values.toArray.sortBy(ss => (ss.stockbrokerAsset.asset, ss.stockbrokerAsset.stockbroker))) {
        val `type` = types(oa.stockbrokerAsset.asset)
        val tag =
          if (`type` != Type.Default) s" [${`type`.code}]"
          else ""

        indentedPrinter.println(s"${oa.stockbrokerAsset.asset}: ${oa.amount.quantity} x ${formatMoney(oa.amount.averagePrice)} - ${oa.stockbrokerAsset.stockbroker}$tag")
      }
    }
  }
}

class VariableIncome(types: Types, debug: Debug) {

  private def processNotes(initialOwnedAssets: OwnedAssets, brokerageNotes: Seq[InputNote]): (Array[YearMonthOutcome], OwnedAssets) = {
    debug.printOwnedAssets(initialOwnedAssets)

    val initialValues = (Array.empty[YearMonthOutcome], initialOwnedAssets)
    val notesByYearMonth = brokerageNotes
      .groupBy(bn => YearMonth.from(bn.date))
      .toList
      .sortBy(_._1)

    val (yearMonthOutcomes, finalAssets) = notesByYearMonth.foldLeft(initialValues) { case ((ymOutcomes, ownedAssets), (yearMonth, notes)) =>
      val ymOutcome = processYearMonthNotes(yearMonth, notes)(ownedAssets)
      (ymOutcomes :+ ymOutcome, ymOutcome.ownedAssets)
    }

    (yearMonthOutcomes, finalAssets)
  }

  private def processYearMonthNotes(yearMonth: YearMonth, notes: Seq[InputNote])(ownedAssets: OwnedAssets): YearMonthOutcome =
    debug.context(yearMonth) {
      val sortedNotes = notes.sortBy {
        case bn: BrokerageNote => (bn.date, bn.stockbroker)
        case en: EventsNote => (en.date, "")
      }
      val initialYMOutcome = YearMonthOutcome(yearMonth, SwingTrade.Zero, Trade.Zero, Trade.Zero, ownedAssets)
      val ymOutcome = sortedNotes.foldLeft(initialYMOutcome) { (ymOutcome, note) =>
        note match {
          case bn: BrokerageNote => processBrokerageNote(bn)(ymOutcome)
          case en: EventsNote => processEventsNote(en)(ymOutcome)
        }
      }

      debug.ifEnabled {
        debug.context("Outcome") {
          debug.println(ymOutcome.swingTrade)
          debug.println(s"Day Trade: ${ymOutcome.dayTrade}")
          debug.println(s"FIIs: ${ymOutcome.fiisTrade}")
          debug.printOwnedAssets(ymOutcome.ownedAssets)
        }
      }

      ymOutcome
    }

  private def processBrokerageNote(note: BrokerageNote)(yearMonthOutcome: YearMonthOutcome): YearMonthOutcome =
    debug.context(s"${note.date} - ${note.stockbroker}") {
      debug.ifEnabled {
        for (n <- note.negotiations) {
          debug.println(n)
        }
        debug.println(s"Costs: ${note.costs}")
        debug.println(s"Value: ${note.totalValue}")
      }

      val operationsAmountsPerStockWithoutCosts = aggregateOperationsAmountsPerStock(note.negotiations)
      val operationsAmountsPerStock = addCosts(note.totalCosts)(operationsAmountsPerStockWithoutCosts)

      operationsAmountsPerStock.foldLeft(yearMonthOutcome) { case (ymOutcome, (asset, opsAmounts)) =>
        val stockbrokerAsset = StockbrokerAsset(note.stockbroker, asset)
        lazy val averagePurchasePriceWithoutCost = operationsAmountsPerStockWithoutCosts(stockbrokerAsset.asset).saleAmount.averagePrice
        processAsset(stockbrokerAsset, opsAmounts, averagePurchasePriceWithoutCost)(ymOutcome)
      }
    }

  private def aggregateOperationsAmountsPerStock(negotiations: Seq[Negotiation]): Map[String, OperationsAmounts] =
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

  private def processAsset(stockbrokerAsset: StockbrokerAsset, opsAmounts: OperationsAmounts, saleAveragePrice: => Double)(ymOutcome: YearMonthOutcome): YearMonthOutcome = {
    val stockType = types(stockbrokerAsset.asset)
    if (!stockType.variableRate) {
      ymOutcome
    } else {
      val dayTradeQuantity = math.min(opsAmounts.purchaseAmount.quantity, opsAmounts.saleAmount.quantity)
      val swingTradeQuantity = opsAmounts.saleAmount.quantity - dayTradeQuantity

      val dayTrade = Trade(
        dayTradeQuantity,
        opsAmounts.purchaseAmount.averagePrice,
        opsAmounts.saleAmount.averagePrice,
      )

      val swingTrade = Trade(
        swingTradeQuantity,
        purchaseAveragePrice = ymOutcome.ownedAssets.get(stockbrokerAsset).fold(0.0)(_.amount.averagePrice),
        saleAveragePrice = opsAmounts.saleAmount.averagePrice,
      )

      val quantityDelta = opsAmounts.purchaseAmount.quantity - opsAmounts.saleAmount.quantity
      val newOwnedAssets = {
        if (quantityDelta > 0) ymOutcome.ownedAssets.addTo(stockbrokerAsset)(opsAmounts.purchaseAmount.focus(_.quantity).replace(quantityDelta))
        else if (quantityDelta < 0) ymOutcome.ownedAssets.removeFrom(stockbrokerAsset)(-quantityDelta)
        else ymOutcome.ownedAssets
      }

      val (exemptableSwingTradeDelta, nonExemptableSwingTradeDelta, dayTradeDelta, fiisTradeDelta) =
        if (stockType.fii) {
          (TradeWithTotalSales.Zero, Trade.Zero, Trade.Zero, swingTrade `add` dayTrade)
        } else if (stockType.exemptable) {
          val swingTradeDelta = TradeWithTotalSales.fromTrade(swingTrade, saleAveragePrice)
          (swingTradeDelta, Trade.Zero, dayTrade, Trade.Zero)
        } else {
          (TradeWithTotalSales.Zero, swingTrade, dayTrade, Trade.Zero)
        }

      ymOutcome
        .focus(_.swingTrade.exemptable).modify(_.add(exemptableSwingTradeDelta))
        .focus(_.swingTrade.nonExemptable).modify(_.add(nonExemptableSwingTradeDelta))
        .focus(_.dayTrade).modify(_.add(dayTradeDelta))
        .focus(_.fiisTrade).modify(_.add(fiisTradeDelta))
        .focus(_.ownedAssets).replace(newOwnedAssets)
    }
  }

  private def processEventsNote(note: EventsNote)(ymOutcome: YearMonthOutcome): YearMonthOutcome =
    debug.context(s"${note.date} Events") {
      ymOutcome.focus(_.ownedAssets).modify { ownedAssets =>
        note.events.foldLeft(ownedAssets) { (ownedAssets, event) =>
          processEvent(event)(ownedAssets)
        }
      }
    }

  private def processEvent(event: Event)(ownedAssets: OwnedAssets): OwnedAssets =
    debug.context(event) {
      val matchingOwnedAssets = ownedAssets.values
        .filter(_.stockbrokerAsset.asset == event.from.asset)
        .toList
      matchingOwnedAssets.foldLeft(ownedAssets) { (ownedAssets, matchingOwnedAsset) =>
        val eventExecutions = matchingOwnedAsset.amount.quantity / event.from.quantity
        val removedQuantity = eventExecutions * event.from.quantity

        debug.context(matchingOwnedAsset) {
          val as = ownedAssets.removeFrom(matchingOwnedAsset.stockbrokerAsset)(removedQuantity)
          event.tos.foldLeft(as) { (ownedAssets, to) =>
            val stockbrokerAsset = StockbrokerAsset(matchingOwnedAsset.stockbrokerAsset.stockbroker, to.asset)
            val amount = Amount(eventExecutions * to.quantity, to.averagePriceDefinition(matchingOwnedAsset.amount.averagePrice))
            val newOwnedAssets = ownedAssets.addTo(stockbrokerAsset)(amount)
            debug.println(s"-> ${newOwnedAssets(stockbrokerAsset)}")
            newOwnedAssets
          }
        }
      }
    }
}
