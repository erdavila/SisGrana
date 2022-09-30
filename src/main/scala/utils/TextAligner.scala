package sisgrana
package utils

import com.softwaremill.quicklens._
import scala.annotation.tailrec

object TextAligner {
  type Anchor = Int

  sealed trait Alignment
  object Alignment {
    case class Left(anchor: Anchor) extends Alignment
    case class Right(anchor: Anchor) extends Alignment
    case class Center(leftAnchor: Anchor, rightAnchor: Anchor) extends Alignment
  }

  case class Chunk(text: String, width: Int, alignment: Alignment)
  object Chunk {
    def apply(text: String, alignment: Alignment): Chunk =
      Chunk(text, text.length, alignment)

    def leftAligned(anchor: Anchor, text: String, width: Anchor): Chunk = Chunk(text, width, Alignment.Left(anchor))
    def leftAligned(anchor: Anchor, text: String): Chunk = Chunk(text, Alignment.Left(anchor))
    def rightAligned(anchor: Anchor, text: String, width: Anchor): Chunk = Chunk(text, width, Alignment.Right(anchor))
    def rightAligned(anchor: Anchor, text: String): Chunk = Chunk(text, Alignment.Right(anchor))
    def centerAligned(leftAnchor: Anchor, rightAnchor: Anchor, text: String, width: Anchor): Chunk = Chunk(text, width, Alignment.Center(leftAnchor, rightAnchor))
    def centerAligned(leftAnchor: Anchor, rightAnchor: Anchor, text: String): Chunk = Chunk(text, Alignment.Center(leftAnchor, rightAnchor))
  }

  case class AlignedChunk(chunk: Chunk, column: Int, leftPaddingWidth: Int)

  abstract class TextAlignerException extends Exception
  case class ConflictingChunksException(chunk1: Chunk, chunk2: Chunk, rowIndex: Int) extends TextAlignerException
  case class InvalidCenterAlignmentException(chunk: Chunk, rowIndex: Int) extends TextAlignerException

  def alignAndRender(rowsChunks: Seq[Seq[Chunk]], paddingChar: Char = ' '): Seq[String] = {
    val alignedChunks = align(rowsChunks)
    render(alignedChunks, paddingChar)
  }

  private case class Row(
    remainingChunks: Seq[Chunk],
    alignedChunks: Vector[AlignedChunk],
    textWidth: Int,
    nextAnchorMinColumn: Int,
  )

  def align(rowsChunks: Seq[Seq[Chunk]]): Seq[Seq[AlignedChunk]] = {
    val rowsSortedChunks = rowsChunks.zipWithIndex.map { case (rowChunks, rowIndex) =>
      sortRowChunks(rowChunks, rowIndex)
    }
    val anchors = rowsSortedChunks.flatMap(_.flatMap(anchorsFrom))
      .distinct
      .sorted

    val initialRows = rowsSortedChunks.map(rowChunks =>
      Row(
        remainingChunks = rowChunks,
        alignedChunks = Vector.empty,
        textWidth = 0,
        nextAnchorMinColumn = 0,
      )
    )

    val initialAnchorsColumns = Map.empty[Anchor, Int]

    val (rows, _) = anchors.foldLeft((initialRows, initialAnchorsColumns)) { case ((rows, anchorsColumns), anchor) =>
      val anchorColumn = rows.map(findAnchorColumnForRow(anchor, anchorsColumns)).max
      val updatedAnchorsColumns = anchorsColumns + (anchor -> anchorColumn)
      val updatedRows = rows.map(alignChunksInRow(anchor, updatedAnchorsColumns))
      (updatedRows, updatedAnchorsColumns)
    }

    rows.map(_.alignedChunks)
  }

  private def sortRowChunks(rowChunks: Seq[Chunk], rowIndex: Int): Seq[Chunk] = {
    for (chunk <- rowChunks) {
      chunk.alignment match {
        case Alignment.Center(leftAnchor, rightAnchor) if leftAnchor >= rightAnchor =>
          throw InvalidCenterAlignmentException(chunk, rowIndex)
        case _ => ()
      }
    }

    val result = rowChunks.sortBy { chunk =>
      chunk.alignment match {
        case Alignment.Left(anchor) => (anchor, 2)
        case Alignment.Right(anchor) => (anchor, 0)
        case Alignment.Center(leftAnchor, _) => (leftAnchor, 1)
      }
    }

    case class ChunkAndAnchors(chunk: Chunk, leftAnchor: Option[Anchor], rightAnchor: Option[Anchor])
    result
      .map(chunk =>
        chunk.alignment match {
          case Alignment.Left(anchor) => ChunkAndAnchors(chunk, Some(anchor), None)
          case Alignment.Right(anchor) => ChunkAndAnchors(chunk, None, Some(anchor))
          case Alignment.Center(leftAnchor, rightAnchor) => ChunkAndAnchors(chunk, Some(leftAnchor), Some(rightAnchor))
        }
      )
      .sliding(2).foreach {
        case Seq(chunkAndAnchors1, chunkAndAnchors2) =>
          lazy val conflicted1 = chunkAndAnchors1.leftAnchor.isDefined && chunkAndAnchors1.leftAnchor == chunkAndAnchors2.leftAnchor
          lazy val conflicted2 = chunkAndAnchors1.rightAnchor.isDefined && chunkAndAnchors1.rightAnchor == chunkAndAnchors2.rightAnchor
          lazy val conflicted3 = (chunkAndAnchors1.rightAnchor, chunkAndAnchors2.leftAnchor) match {
            case (Some(anchor1), Some(anchor2)) if anchor2 < anchor1 => true
            case _ => false
          }
          lazy val conflicted4 = (chunkAndAnchors1.rightAnchor, chunkAndAnchors2.rightAnchor) match {
            case (Some(anchor1), Some(anchor2)) if anchor2 < anchor1 => true
            case _ => false
          }

          if (conflicted1 || conflicted2 || conflicted3 || conflicted4) {
            throw ConflictingChunksException(chunkAndAnchors1.chunk, chunkAndAnchors2.chunk, rowIndex)
          }
        case _ => ()
      }

    result
  }

  private def anchorsFrom(chunk: Chunk): Set[Anchor] =
    chunk.alignment match {
      case Alignment.Left(anchor) => Set(anchor)
      case Alignment.Right(anchor) => Set(anchor)
      case Alignment.Center(leftAnchor, rightAnchor) => Set(leftAnchor, rightAnchor)
    }

  private def findAnchorColumnForRow(anchor: Anchor, anchorsColumns: Map[Anchor, Int])(row: Row): Int =
    row.remainingChunks.headOption match {
      case Some(chunk) =>
        chunk.alignment match {
          case Alignment.Left(`anchor`) => row.nextAnchorMinColumn
          case Alignment.Right(`anchor`) => row.nextAnchorMinColumn + chunk.width
          case Alignment.Center(leftAnchor, _) if leftAnchor == anchor => row.nextAnchorMinColumn
          case Alignment.Center(leftAnchor, rightAnchor) if rightAnchor == anchor => anchorsColumns(leftAnchor) + chunk.width
          case _ => 0
        }
      case None => 0
    }

  @tailrec
  private def alignChunksInRow(anchor: Anchor, anchorsColumns: Map[Anchor, Int])(row: Row): Row =
    row.remainingChunks.headOption match {
      case Some(chunk) =>
        def updatedRow(chunkLeftColumn: Int, chunkRightColumn: Int): Row = {
          val alignedChunk = AlignedChunk(
            chunk = chunk,
            column = chunkLeftColumn,
            leftPaddingWidth = chunkLeftColumn - row.textWidth,
          )
          row
            .modify(_.remainingChunks).using(_.tail)
            .modify(_.alignedChunks).using(_ :+ alignedChunk)
            .modify(_.textWidth).setTo(chunkRightColumn)
            .modify(_.nextAnchorMinColumn).setTo(chunkRightColumn)
        }

        chunk.alignment match {
          case Alignment.Left(`anchor`) =>
            val anchorColumn = anchorsColumns(anchor)
            val chunkLeftColumn = anchorColumn
            val chunkRightColumn = chunkLeftColumn + chunk.width
            updatedRow(chunkLeftColumn, chunkRightColumn)
          case Alignment.Right(`anchor`) =>
            val anchorColumn = anchorsColumns(anchor)
            val chunkRightColumn = anchorColumn
            val chunkLeftColumn = chunkRightColumn - chunk.width
            val updRow = updatedRow(chunkLeftColumn, chunkRightColumn)
            alignChunksInRow(anchor, anchorsColumns)(updRow)
          case Alignment.Center(leftAnchor, rightAnchor) if rightAnchor == anchor =>
            val leftAnchorColumn = anchorsColumns(leftAnchor)
            val rightAnchorColumn = anchorsColumns(rightAnchor)
            val chunkLeftColumn = leftAnchorColumn + (rightAnchorColumn - leftAnchorColumn - chunk.width) / 2
            val chunkRightColumn = chunkLeftColumn + chunk.width
            val updRow = updatedRow(chunkLeftColumn, chunkRightColumn)
              .modify(_.nextAnchorMinColumn).setTo(rightAnchorColumn)
            alignChunksInRow(anchor, anchorsColumns)(updRow)
          case _ => row
        }
      case None => row
    }

  def render(alignedRowsChunks: Seq[Seq[AlignedChunk]], paddingChar: Char = ' '): Seq[String] =
    alignedRowsChunks.map(renderRowChunks(_, paddingChar))

  def renderRowChunks(alignedRowChunks: Seq[AlignedChunk], paddingChar: Char = ' '): String =
    alignedRowChunks.map(renderChunk(_, paddingChar)).mkString

  def renderChunk(alignedChunk: AlignedChunk, paddingChar: Char = ' '): String =
    paddingChar.toString * alignedChunk.leftPaddingWidth ++ alignedChunk.chunk.text
}
