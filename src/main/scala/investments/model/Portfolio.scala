package sisgrana
package investments.model

import com.softwaremill.quicklens._
import investments.model.LocalDateSupport._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import java.time.LocalDate
import utils.DateRange.Mode.DayChange
import utils.DateRanges

case class Portfolio(name: String, content: Map[StockbrokerAsset, DateRanges]) {
  def add(stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate, endDate: LocalDate): Portfolio = {
    val newContent = content.updatedWith(stockbrokerAsset) { dateRangesOpt =>
      val addedRange = DateRanges.single(beginDate, endDate)
      val newDateRanges = dateRangesOpt match {
        case Some(dateRanges) => dateRanges `union` addedRange
        case None => addedRange
      }
      Some(newDateRanges)
    }

    this.copy(content = newContent)
  }

  def remove(stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate, endDate: LocalDate): Portfolio = {
    val newContent = content.updatedWith(stockbrokerAsset) { dateRangesOpt =>
      dateRangesOpt.flatMap { dateRanges =>
        val removedRange = DateRanges.single(beginDate, endDate)
        val newDateRanges = dateRanges `diff` removedRange
        Option.when(newDateRanges.nonEmpty)(newDateRanges)
      }
    }

    this.copy(content = newContent)
  }

  def remove(stockbrokerAsset: StockbrokerAsset): Portfolio =
    this.modify(_.content).using(_ - stockbrokerAsset)
}

object Portfolio {
  def listNames(): Iterable[String] =
    ctx.run(
      query[PortfolioAssetDateRange]
        .map(_.portfolioName)
        .distinct
    )

  def load(name: String): Option[Portfolio] = {
    val assetDateRanges = ctx.run(
      query[PortfolioAssetDateRange]
        .filter(_.portfolioName == lift(name))
    )
    Option.when(assetDateRanges.nonEmpty) {
      val content = assetDateRanges.groupMapReduce(_.stockbrokerAsset)(assetDateRange => DateRanges.single(assetDateRange.dateRange))(_ `union` _)
      Portfolio(name, content)
    }
  }

  def save(portfolio: Portfolio): Unit =
    ctx.transaction {
      val assetDateRanges =
        for {
          (stockbrokerAsset, dateRanges) <- portfolio.content
          dateRange <- dateRanges.indexedSeq
        } yield PortfolioAssetDateRange(portfolio.name, stockbrokerAsset, dateRange)

      ctx.run(
        query[PortfolioAssetDateRange]
          .filter(_.portfolioName == lift(portfolio.name))
          .delete
      )

      ctx.run(
        for (dateRange <- liftQuery(assetDateRanges)) {
          query[PortfolioAssetDateRange].insert(dateRange)
        }
      )
    }
}
