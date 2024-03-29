package sisgrana
package investments.commands.incomeRate

import investments.AssetType
import investments.commands.portfolio.PortfolioMain
import investments.model._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import java.time.LocalDate
import scala.annotation.tailrec
import utils.{BrNumber, DateRange, DateRanges, IndentedPrinter}

object IncomeRateMain extends LocalDateSupport {
  private[incomeRate] case class Position(quantity: Int, convertedTo: Option[ConvertedTo]) {
    require(quantity > 0)
  }

  private case class QuantityAndQuote(quantity: Double, quote: Double) {
    def value: Double = quantity * quote
  }

  private case class InitialAndFinal[A](initial: A, `final`: A)

  private[incomeRate] case class DataSubPeriod[A](data: Vector[(DateRange, A)]) {
    require(data.nonEmpty)
    def beginDate: LocalDate = data.head._1.beginDate
    def endDate: LocalDate = data.last._1.endDate
    def dateRange: DateRange = DateRange(beginDate, endDate)
    def :+ (elem: (DateRange, A)): DataSubPeriod[A] = DataSubPeriod(data :+ elem)
  }

  private object DataSubPeriod {
    def from[A](singleData: (DateRange, A)): DataSubPeriod[A] = DataSubPeriod(Vector(singleData))
  }

  def main(args: Array[String]): Unit = {
    val (resolvePortfolio, periodSpec, positiveFilters, negativeFilters) = ArgsParser.parse(args)

    for (period <- periodSpec.periods) {
      val filtersResolver = new FiltersResolver(period)
      val portfolioContent = filtersResolver.resolve(positiveFilters, negativeFilters)
        .filter {
          case (stockbrokerAsset, _) =>
            !AssetType.Resolver.isOption(stockbrokerAsset.asset)
        }

      if (resolvePortfolio) {
        val printer = new IndentedPrinter
        PortfolioMain.printPortfolioContent(portfolioContent, printer)
      } else {
        calculateAndShowIncomeRate(period, portfolioContent)
      }
    }
  }

  private def calculateAndShowIncomeRate(period: Period, portfolioContent: PortfolioContent): Unit = {
    val quantities = queryQuantities(portfolioContent)
    val dateRangesQuantities = regularizeDateRanges(quantities)
    val quantitiesAndQuotes = toQuantitiesAndQuotes(dateRangesQuantities)
    val dateRangesIncomeRates =
      for {
        (dateRange, qqs) <- quantitiesAndQuotes
        rate = calculateIncomeRate(qqs)
      } yield (dateRange, rate)

    val dataSubPeriods = joinSubPeriods(dateRangesIncomeRates)
    val incomeRateSubPeriods =
      for {
        subPeriod <- dataSubPeriods
        subPeriodRates = subPeriod.data.map(_._2)
        incomeRate = subPeriodRates.reduce((r1, r2) => r1 * r2 + r1 + r2)
      } yield subPeriod.dateRange -> incomeRate

    incomeRateSubPeriods match {
      case Seq((`period`.dateRange, rate)) => showRate(period, rate)
      case _ =>
        for ((dateRange, rate) <- fillSubPeriods(incomeRateSubPeriods, period.dateRange)) {
          rate match {
            case Some(rate) => showRate(Period.DateRange(dateRange), rate)
            case None => println(s"${dateRange.beginDate} a ${dateRange.endDate}: Nenhum ativo com quantia positiva")
          }
        }
    }
  }

  private def queryQuantities(portfolio: PortfolioContent): Map[StockbrokerAsset, Seq[(DateRange, Position)]] = {
    for {
      (stockbrokerAsset, dateRanges) <- portfolio
      qs = queryQuantities(stockbrokerAsset, dateRanges)
      if qs.nonEmpty
    } yield stockbrokerAsset -> qs
  }

  private def queryQuantities(stockbrokerAsset: StockbrokerAsset, dateRanges: DateRanges): Seq[(DateRange, Position)] =
    for {
      dateRange <- dateRanges.indexedSeq
      q <- queryQuantities(stockbrokerAsset, dateRange)
    } yield q

  private[incomeRate] def queryQuantities(stockbrokerAsset: StockbrokerAsset, dateRange: DateRange): Seq[(DateRange, Position)] = {
    val assetPeriods = ctx.run(
      AssetPeriod.inDateRangeQuery(dateRange)
        .filter(_.asset == lift(stockbrokerAsset.asset))
        .filter(_.stockbroker == lift(stockbrokerAsset.stockbroker))
        .filter(_.beginDate < lift(dateRange.endDate))
        .filter(_.endDate > lift(dateRange.beginDate))
        .filter(_.resultingPositionQuantity > 0)
        .sortBy(_.beginDate)
    )

    assetPeriods
      .map { ap =>
        val position = Position(ap.resultingPosition.quantity, ap.convertedTo)
        (ap.dateRange, position)
      }
      .pipeWhenMatchedSelf {
        case (dr, position) +: tail if dr.beginDate `isBefore` dateRange.beginDate =>
          val newDR = dr.copy(beginDate = dateRange.beginDate)
          (newDR, position) +: tail
      }
      .pipeWhenMatchedSelf {
        case init :+ ((dr, position)) if dr.endDate `isAfter` dateRange.endDate =>
          val newDR = dr.copy(endDate = dateRange.endDate)
          val newPosition = position.copy(convertedTo = None)
          init :+ ((newDR, newPosition))
      }
  }

  private[incomeRate] def regularizeDateRanges(quantities: Map[StockbrokerAsset, Seq[(DateRange, Position)]]): Seq[(DateRange, Map[StockbrokerAsset, Position])] = {
    val allDates = (
      for {
        (_, qs) <- quantities
        (dateRange, _) <- qs
        date <- Set(dateRange.beginDate, dateRange.endDate)
      } yield date
    ).toSeq.distinct.sorted

    @tailrec
    def splitDateRanges(qs: Seq[(DateRange, Position)], dates: Seq[LocalDate], results: Vector[(DateRange, Position)]): Seq[(DateRange, Position)] = {
      assert(dates.nonEmpty)
      qs match {
        case (dateRange, position) +: rest =>
          val (dateRanges, remainingDates) = splitDateRange(dateRange, dates)
          assume(dateRanges.nonEmpty)
          val initResults = for (dr <- dateRanges.init) yield (dr, position.copy(convertedTo = None))
          val lastResult = (dateRanges.last, position)
          splitDateRanges(rest, remainingDates, results :++ initResults :+ lastResult)
        case _ =>
          results
      }
    }

    def splitDateRange(dateRange: DateRange, dates: Seq[LocalDate]): (Seq[DateRange], Seq[LocalDate]) = {
      assert(!(dates.head `isAfter` dateRange.beginDate))

      val (beforeEndDates, fromEndDates) = dates
        .dropWhile(_ `isBefore` dateRange.beginDate)
        .span(_ `isBefore` dateRange.endDate)
      assert(beforeEndDates.head == dateRange.beginDate)
      assert(fromEndDates.head == dateRange.endDate)

      val newDateRanges =
        for (Seq(beginDate, endDate) <- (beforeEndDates :+ dateRange.endDate).sliding(2))
          yield DateRange(beginDate, endDate)

      (newDateRanges.toSeq, fromEndDates)
    }

    case class Group(dateRange: DateRange, stockbrokerAsset: StockbrokerAsset, position: Position)

    val groups =
      for {
        (stockbrokerAsset, qs) <- quantities
        (dateRange, position) <- splitDateRanges(qs, allDates, Vector.empty)
      } yield Group(dateRange, stockbrokerAsset, position)

    groups
      .groupMapReduce(_.dateRange)(g => Map(g.stockbrokerAsset -> g.position)) { (map1, map2) =>
        assert((map1.keySet `intersect` map2.keySet).isEmpty)
        map1 ++ map2
      }
      .toSeq
      .sortBy(_._1.beginDate)
  }

  private def toQuantitiesAndQuotes(quantities: Seq[(DateRange, Map[StockbrokerAsset, Position])]): Seq[(DateRange, Seq[InitialAndFinal[QuantityAndQuote]])] =
    for {
      (dateRange, positions) <- quantities
      qqs = toQuantitiesAndQuotes(dateRange, positions)
    } yield (dateRange, qqs)

  private def toQuantitiesAndQuotes(dateRange: DateRange, positions: Map[StockbrokerAsset, Position]): Seq[InitialAndFinal[QuantityAndQuote]] = {
    case class AssetAndQuantity(asset: String, quantity: Double)

    val assetsAndQuantities =
      for {
        (stockbrokerAsset, position) <- positions.to(Seq)
        initialAQ = AssetAndQuantity(stockbrokerAsset.asset, position.quantity)
        finalAQ = position.convertedTo match {
          case Some(convertedTo) => AssetAndQuantity(convertedTo.asset, convertedTo.quantity)
          case None => initialAQ
        }
      } yield InitialAndFinal(initialAQ, finalAQ)

    def toQuantityAndQuoteSeq(date: LocalDate, assetAndQuantitySeq: Seq[AssetAndQuantity]): Seq[QuantityAndQuote] = {
      val assets = assetAndQuantitySeq.map(_.asset).to(Set)
      val quotes = getQuotes(date, assets)
      for {
        aq <- assetAndQuantitySeq
        quote = quotes(aq.asset)
      } yield QuantityAndQuote(aq.quantity, quote)
    }

    val initialQQs = toQuantityAndQuoteSeq(dateRange.beginDate, assetsAndQuantities.map(_.initial))
    val finalQQs = toQuantityAndQuoteSeq(dateRange.endDate, assetsAndQuantities.map(_.`final`))

    for ((i, f) <- initialQQs `zip` finalQQs)
      yield InitialAndFinal(i, f)
  }

  private def getQuotes(date: LocalDate, assets: Set[String]): Map[String, Double] = {
    val result = ctx.run(
      for {
        q <- query[AssetQuote]
        if q.date == lift(date)
        if liftQuery(assets).contains(q.asset)
      } yield (q.asset, q.closePrice)
    )

    val assetsMissingQuote = assets -- result.map(_._1)
    if (assetsMissingQuote.nonEmpty) {
      throw new Exception(s"Cotação não encontrada em $date para ${assets.toSeq.sorted.mkString(", ")}")
    }

    result.to(Map)
  }

  private def calculateIncomeRate(quantitiesAndQuotes: Seq[InitialAndFinal[QuantityAndQuote]]): Double = {
    def value(f: InitialAndFinal[QuantityAndQuote] => QuantityAndQuote) =
      quantitiesAndQuotes
        .map(qqs => f(qqs).value)
        .sum

    val initialValue = value(_.initial)
    val finalValue = value(_.`final`)

    (finalValue - initialValue) / initialValue
  }

  private[incomeRate] def joinSubPeriods[A](dateRangesData: Seq[(DateRange, A)]): Seq[DataSubPeriod[A]] = {
    @tailrec
    def loop(dateRangesData: Seq[(DateRange, A)], subPeriods: Vector[DataSubPeriod[A]]): Seq[DataSubPeriod[A]] =
      dateRangesData match {
        case data +: rest =>
          val (subPeriod, newRest) = extendSubPeriod(DataSubPeriod.from(data), rest)
          loop(newRest, subPeriods :+ subPeriod)
        case _ => subPeriods
      }

    @tailrec
    def extendSubPeriod(subPeriod: DataSubPeriod[A], dateRangesData: Seq[(DateRange, A)]): (DataSubPeriod[A], Seq[(DateRange, A)]) =
      dateRangesData match {
        case (h@ (dateRange, _)) +: t if dateRange.beginDate == subPeriod.endDate =>
          extendSubPeriod(subPeriod :+ h, t)
        case _ =>
          (subPeriod, dateRangesData)
      }

    loop(dateRangesData, Vector.empty)
  }

  private[incomeRate] def fillSubPeriods[A](subPeriods: Seq[(DateRange, A)], dateRange: DateRange): Seq[(DateRange, Option[A])] = {
    @tailrec
    def loop(subPeriods: Seq[(DateRange, A)], date: LocalDate, results: Seq[(DateRange, Option[A])]): Seq[(DateRange, Option[A])] = {
      def existing(subPeriod: (DateRange, A)) = subPeriod._1 -> Some(subPeriod._2)
      def filling(endDate: LocalDate) = DateRange(date, endDate) -> None
      subPeriods match {
        case sub +: t if sub._1.beginDate == date => loop(t, sub._1.endDate, results :+ existing(sub))
        case sub +: t =>
          assert(date `isBefore` sub._1.beginDate)
          loop(t, sub._1.endDate, results :+ filling(sub._1.beginDate) :+ existing(sub))
        case _ if date == dateRange.endDate => results
        case _ => results :+ filling(dateRange.endDate)
      }
    }

    loop(subPeriods, dateRange.beginDate, Vector.empty)
  }

  private def showRate(period: Period, rate: Double): Unit = {
    val yearPercentSuffix = "a.a."
    val monthPercentSuffix = "a.m."
    period match {
      case Period.Year(year) =>
        val yearPercent = BrNumber.formatPercent(rate) ++ yearPercentSuffix
        val monthPercent = BrNumber.formatPercent(convertRate(rate, 12, 1)) ++ monthPercentSuffix
        println(s"$year: $yearPercent ($monthPercent)")
      case Period.YearRange(beginYear, endYear) =>
        val yearCount = period.lengthInChronoUnits
        val percent = BrNumber.formatPercent(rate)
        val yearPercent = BrNumber.formatPercent(convertRate(rate, yearCount, 1)) ++ yearPercentSuffix
        val monthPercent = BrNumber.formatPercent(convertRate(rate, yearCount * 12, 1)) ++ monthPercentSuffix
        println(s"$beginYear a $endYear: $percent ($yearPercent; $monthPercent)")
      case Period.Month(yearMonth) =>
        val monthPercent = BrNumber.formatPercent(rate) ++ monthPercentSuffix
        val yearPercent = BrNumber.formatPercent(convertRate(rate, 1, 12)) ++ yearPercentSuffix
        println(s"$yearMonth: $monthPercent ($yearPercent)")
      case Period.MonthRange(beginYearMonth, endYearMonth) =>
        val monthCount = period.lengthInChronoUnits
        val percent = BrNumber.formatPercent(rate)
        val yearPercent = BrNumber.formatPercent(convertRate(rate, monthCount, 12)) ++ yearPercentSuffix
        val monthPercent = BrNumber.formatPercent(convertRate(rate, monthCount, 1)) ++ monthPercentSuffix
        println(s"$beginYearMonth a $endYearMonth: $percent ($yearPercent; $monthPercent)")
      case Period.DateRange(beginDate, endDate) =>
        val dayCount = period.lengthInChronoUnits
        val percent = BrNumber.formatPercent(rate)
        val DaysPerYear = 365.2425
        val monthPercent = BrNumber.formatPercent(convertRate(rate, dayCount, DaysPerYear / 12)) ++ monthPercentSuffix
        val yearPercent = BrNumber.formatPercent(convertRate(rate, dayCount, DaysPerYear)) ++ yearPercentSuffix
        println(s"$beginDate a $endDate: $percent ($monthPercent; $yearPercent)")
    }
  }

  private def convertRate(rate: Double, from: Double, to: Double): Double =
    math.pow(rate + 1, to / from) - 1
}
