package sisgrana
package investments.commands.funds.operations.evolutionOf

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.commands.funds.ChunkMaker
import java.time.{LocalDate, YearMonth}
import utils.AnsiString
import utils.AnsiString.StringOps
import utils.TextAligner.Chunk
import utils.Traversing._
import scala.annotation.tailrec

object EvolutionChunkMaker extends ChunkMaker {
  sealed trait EvolutionItem {
    def shareAmount: BigDecimal
  }

  object EvolutionItem {
    case class PreviousMonth(previousMonth: YearMonth, shareAmount: BigDecimal) extends EvolutionItem

    case class CurrentMonth(date: LocalDate, shareAmount: BigDecimal, sharePrice: Double) extends EvolutionItem {
      val moneyValue: Double = shareAmount.toDouble * sharePrice
    }
  }

  private object EvolutionAnchors {
    val Leftmost = 0
    val SharePrice = 1
    val ShareAmount = 2
    val MoneyValue = 3
  }

  def makeChunks(items: Seq[EvolutionItem]): Seq[Seq[Chunk]] =
    items.traverse(Vector.empty[EvolutionItem]) { (items, item) =>
      val allItems = items :+ item
      val itemRowChunks = toEvolutionItemChunks(item)
      val totalRowChunks = Option.when(items.nonEmpty) {
        toEvolutionTotalChunks(allItems)
      }

      if (item.shareAmount >= 0) {
        (allItems, itemRowChunks +: totalRowChunks.toSeq)
      } else {
        @tailrec
        def updateItems(items: Vector[EvolutionItem], shareAmount: BigDecimal): Vector[EvolutionItem] = {
          require(shareAmount < 0)
          val updatedHeadItem = items.head.modify(_.shareAmount).using(_ + shareAmount)
          if (updatedHeadItem.shareAmount > 0) {
            updatedHeadItem +: items.tail
          } else if (updatedHeadItem.shareAmount < 0) {
            updateItems(items.tail, updatedHeadItem.shareAmount)
          } else {
            items.tail
          }
        }

        val updatedItems = updateItems(items, item.shareAmount)

        val resetRowsChunks = {
          val separatorRowChunks = Seq(Chunk.leftAligned(EvolutionAnchors.Leftmost, "-" * 5))
          val updatedItemsRowsChunks = updatedItems.map(toEvolutionItemChunks)
          val totalRowChunks = toEvolutionTotalChunks(updatedItems)

          separatorRowChunks +: updatedItemsRowsChunks :+ totalRowChunks
        }

        (updatedItems, itemRowChunks +: (totalRowChunks ++: resetRowsChunks))
      }
    }(_ => None)

  private def toEvolutionItemChunks(item: EvolutionItem): Seq[Chunk] = {
    item match {
      case EvolutionItem.PreviousMonth(previousMonth, shareAmount) =>
        toRowChunks(
          time = Some(previousMonth.toString),
          sharePrice = Some("?"),
          shareAmount = colorize(shareAmount)(formatShareAmountChange),
          moneyValue = "?",
        )
      case i@EvolutionItem.CurrentMonth(date, shareAmount, sharePrice) =>
        toRowChunks(
          time = Some(date.toString),
          sharePrice = Some(formatSharePrice(sharePrice)),
          shareAmount = colorize(shareAmount)(formatShareAmountChange),
          moneyValue = formatMoneyChange(i.moneyValue),
        )
    }
  }

  private def toEvolutionTotalChunks(items: Seq[EvolutionItem]): Seq[Chunk] = {
    val moneyValueSum = items
      .map {
        case _: EvolutionItem.PreviousMonth => None
        case i: EvolutionItem.CurrentMonth => Some(i.moneyValue)
      }
      .foldLeft(Option(0.0)) { (moneyValue1, moneyValue2) => (moneyValue1, moneyValue2).mapN(_ + _) }

    toBoldChunks(
      toRowChunks(
        time = None,
        sharePrice = None,
        shareAmount = formatShareAmount(items.map(_.shareAmount).sum),
        moneyValue = moneyValueSum.fold("?")(formatMoney),
      )
    )
  }

  private def toRowChunks(time: Option[String], sharePrice: Option[String], shareAmount: AnsiString, moneyValue: String): Seq[Chunk] = {
    val Spacing = "  "
    val shareAmountText = Spacing ++ shareAmount

    val timeChunk = for (time <- time) yield Chunk.leftAligned(EvolutionAnchors.Leftmost, time)
    val sharePriceChunk = for (sharePrice <- sharePrice) yield Chunk.rightAligned(EvolutionAnchors.SharePrice, Spacing ++ sharePrice)
    val shareAmountChunk = Chunk.rightAligned(EvolutionAnchors.ShareAmount, shareAmountText.toString, shareAmountText.length)
    val moneyValueChunk = Chunk.rightAligned(EvolutionAnchors.MoneyValue, Spacing ++ moneyValue)

    timeChunk.toSeq ++ sharePriceChunk ++ Seq(shareAmountChunk, moneyValueChunk)
  }
}
