package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement.{FundsMonthStatementFileReader, FundsStatement}
import java.time.{LocalDate, YearMonth}
import utils.TextAligner.Chunk
import utils.quoted

object MonthStatementChunkMaker extends ChunkMaker {
  def makeChunks(statement: FundsStatement): Seq[Seq[Chunk]] = {
    require((statement.noPriceDates `union` statement.entries.keySet).map(YearMonth.from).sizeIs <= 1, "All dates must be on the same YearMonth")
    require((statement.noPriceDates `intersect` statement.entries.keySet).isEmpty, "Entries dates and noPriceDates must not have dates in common")

    val initialEntriesChunks = makeInitialEntriesChunks(statement.initialEntries)
    val entriesChunksByDay = makeEntriesChunksByDay(statement.entries)
    val noPriceDatesChunksByDay = makeNoPriceDatesChunksByDay(statement.noPriceDates)
    assert((entriesChunksByDay.keySet `intersect` noPriceDatesChunksByDay.keySet).isEmpty)

    val SpacingRow = Seq.empty

    val spacedInitialEntriesChunks = seqIf(initialEntriesChunks.nonEmpty) {
      SpacingRow +: initialEntriesChunks
    }

    val chunksByDay = entriesChunksByDay ++ noPriceDatesChunksByDay.view.mapValues(Seq(_))
    val spacedDaysChunks = chunksByDay.toSeq
      .sortBy { case (day, _) => day }
      .flatMap { case (_, chunks) => SpacingRow +: chunks }

    Seq(headerRowChunks) ++ spacedInitialEntriesChunks ++ spacedDaysChunks
  }

  private val Separator = "  "

  private object Anchors {
    val Leftmost = 0
    val Fund = 1
    val Separator = 2
    val SharePrice = 3
    val ShareAmount = 4
  }

  private def headerRowChunks: Seq[Chunk] = Seq(
    Chunk.leftAligned(Anchors.Leftmost, s"# Day${Separator}Fund"),
    Chunk.leftAligned(Anchors.Separator, Separator),
    Chunk.rightAligned(Anchors.SharePrice, "Share Price"),
    Chunk.rightAligned(Anchors.ShareAmount, s"${Separator}Share Change"),
    Chunk.leftAligned(Anchors.ShareAmount, s"${Separator}Note"),
  )

  private def makeInitialEntriesChunks(initialEntries: Map[String, FundsStatement.InitialEntry]): Seq[Seq[Chunk]] =
    for ((fund, initialEntry) <- initialEntries.toSeq.sortBy { case (fund, _) => fund })
      yield makeRowChunks(FundsMonthStatementFileReader.InitialEntryMarker, fund, Some(initialEntry.sharePrice), Some(initialEntry.shareAmount), initialEntry.note)

  private def makeEntriesChunksByDay(entries: Map[LocalDate, Map[String, FundsStatement.Entry]]): Map[Int, Seq[Seq[Chunk]]] =
    for {
      (date, entriesByFund) <- entries
      day = date.getDayOfMonth
    } yield day -> makeEntriesChunks(day, entriesByFund)

  private def makeEntriesChunks(day: Int, entries: Map[String, FundsStatement.Entry]): Seq[Seq[Chunk]] =
    for ((fund, entry) <- entries.toSeq.sortBy { case (fund, _) => fund })
      yield makeRowChunks(day.toString, fund, Some(entry.sharePrice), entry.shareAmountChange, entry.note)

  private def makeRowChunks(
    dayString: String,
    text: String,
    sharePrice: Option[Double] = None,
    shareAmountChange: Option[BigDecimal] = None,
    note: Option[String] = None,
  ): Seq[Chunk] = {
    val shareAmountChangeText = shareAmountChange
      .map(shareAmountChange => EightDigitsNumberFormat.format(shareAmountChange.toDouble))
      .orElse(Option.when(note.isDefined)("0"))

    Seq(
      Chunk.leftAligned(Anchors.Leftmost, dayString),
      Chunk.leftAligned(Anchors.Fund, Separator ++ ssvQuoted(text)),
    ) ++ sharePrice.toSeq.flatMap(sharePrice =>
      Seq(
        Chunk.leftAligned(Anchors.Separator, Separator),
        Chunk.rightAligned(Anchors.SharePrice, EightDigitsNumberFormat.format(sharePrice)),
      ) ++ shareAmountChangeText.toSeq.map(shareAmountChangeText =>
        Chunk.rightAligned(Anchors.ShareAmount, Separator ++ shareAmountChangeText)
      ) ++ note.map(note =>
        Chunk.leftAligned(Anchors.ShareAmount, Separator ++ ssvQuoted(note))
      )
    )
  }

  private val Quote = '"'.toString

  private def ssvQuoted(str: String): String =
    quoted(str.replaceAll(Quote, Quote ++ Quote))

  private def makeNoPriceDatesChunksByDay(noPriceDates: Set[LocalDate]): Map[Int, Seq[Chunk]] =
    noPriceDates
      .map { date =>
        val day = date.getDayOfMonth
        val chunks = makeRowChunks(day.toString, FundsMonthStatementFileReader.NoPricesMarker)
        day -> chunks
      }
      .toMap
}
