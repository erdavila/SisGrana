package sisgrana
package investments.commands.portfolio

import investments.model.{Portfolio, StockbrokerAsset}
import java.time.LocalDate
import utils.{Exit, IndentedPrinter}

object PortfolioMain {
  private val MaxDate = LocalDate.of(9999, 12, 31)

  def main(args: Array[String]): Unit = {
    val operationArguments = ArgsParser.parse(args)
    operationArguments match {
      case OperationArguments.List => list()
      case OperationArguments.Show(portfolioName) => show(portfolioName)
      case OperationArguments.Add(portfolioName, stockbrokerAsset, beginDate, endDateOpt) => add(portfolioName, stockbrokerAsset, beginDate, endDateOpt)
      case OperationArguments.Remove(portfolioName, stockbrokerAsset, beginDateOpt, endDateOpt) => remove(portfolioName, stockbrokerAsset, beginDateOpt, endDateOpt)
    }
  }

  private def list(): Unit =
    for (name <- Portfolio.listNames()) {
      println(name)
    }

  private def show(portfolioName: String): Unit =
    Portfolio.load(portfolioName) match {
      case Some(portfolio) => printPortfolio(portfolio)
      case None => nonExistingPortfolioError()
    }

  private def add(portfolioName: String, stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate, endDateOpt: Option[LocalDate]): Unit = {
    val portfolio = Portfolio.load(portfolioName)
      .getOrElse(Portfolio(portfolioName))

    val newPortfolio = portfolio.add(stockbrokerAsset, beginDate, endDateOpt.getOrElse(MaxDate))

    Portfolio.save(newPortfolio)

    printPortfolio(newPortfolio)
  }

  private def remove(portfolioName: String, stockbrokerAsset: StockbrokerAsset, beginDateOpt: Option[LocalDate], endDateOpt: Option[LocalDate]): Unit = {
    val portfolio = Portfolio.load(portfolioName)
      .getOrElse(nonExistingPortfolioError())

    val newPortfolio =
      beginDateOpt match {
        case Some(beginDate) =>
          val endDate = endDateOpt.getOrElse(MaxDate)
          portfolio.remove(stockbrokerAsset, beginDate, endDate)
        case None => portfolio.remove(stockbrokerAsset)
      }

    Portfolio.save(newPortfolio)

    if (newPortfolio.content.nonEmpty) {
      printPortfolio(newPortfolio)
    } else {
      println("Carteira removida")
    }
  }

  private def printPortfolio(portfolio: Portfolio): Unit = {
    val printer = new IndentedPrinter
    printer.context(portfolio.name) {
      val dateRangesByAssetByStockbroker =
        portfolio.content
          .groupMap(_._1.stockbroker) { case (stockbrokerAsset, dateRanges) => stockbrokerAsset.asset -> dateRanges }
          .view
          .mapValues(_.toMap)
          .toMap
      for ((stockbroker, dateRangesByAsset) <- dateRangesByAssetByStockbroker.toSeq.sortBy(_._1)) {
        printer.context(stockbroker) {
          for ((asset, dateRanges) <- dateRangesByAsset.toSeq.sortBy(_._1)) {
            printer.context(asset) {
              for (dateRange <- dateRanges.indexedSeq) {
                printer.println(s"${dateRange.beginDate} a ${dateRange.endDate}")
              }
            }
          }
        }
      }
    }
  }

  private def nonExistingPortfolioError(): Nothing =
    Exit.withErrorMessage(_.println("Carteira não existente"))
}
