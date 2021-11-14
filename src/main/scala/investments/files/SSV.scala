package sisgrana
package investments.files

import com.softwaremill.quicklens._
import java.io.{InputStream, OutputStream, PrintWriter}
import scala.io.Source
import utils.Traversing._
import utils.use

object SSV {
  type LineValues = Seq[String]

  case class UnexpectedCharacterException(char: Char, lineNumber: Int, columnNumber: Int) extends Exception {
    override def getMessage: String =
      s"Unexpected character '$char' (\\u${char.toHexString}) at $lineNumber:$columnNumber"
  }

  class InterruptedContentException extends Exception

  def readFrom(inputStream: InputStream): Iterator[LineValues] = {
    val source = Source.fromInputStream(inputStream)
    fromChars(source)
  }

  private[files] def fromChars(chars: Iterator[Char]): Iterator[LineValues] = {
    case class Data(lineValues: LineValues, value: String, lineNumber: Int, columnNumber: Int) {
      def appendToValue(char: Char): Data =
        this.modify(_.value).using(_ :+ char)

      def commitValue(): Data =
        this
          .modify(_.lineValues).using(_ :+ this.value)
          .modify(_.value).setTo("")

      def commitLineValues(): (Data, LineValues) = {
        val newData = this.modify(_.lineValues).setTo(Vector.empty)
        (newData, this.lineValues)
      }

      def incrementLineNumber(): Data =
        this
          .modify(_.lineNumber).using(_ + 1)
          .modify(_.columnNumber).setTo(1)

      def incrementColumnNumber(): Data =
        this.modify(_.columnNumber).using(_ + 1)
    }

    sealed trait State {
      def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])]

      final def consume(data: Data)(char: Char): ((State, Data), Option[LineValues]) =
        consume.applyOrElse((char, data), { _: (Char, Data) =>
          throw UnexpectedCharacterException(char, data.lineNumber, data.columnNumber)
        })

      def finalize(data: Data): Option[LineValues]
    }

    case object ExpectingLine extends State {
      override val consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case (' ', data) => (SkippingSpace -> data.incrementColumnNumber(), None)
        case ('\n', data) =>
          val (newData, lineValues) = data.commitLineValues()
          (this -> newData.incrementLineNumber(), Some(lineValues))
        case ('"', data) => (ConsumingQuotedValue -> data.incrementColumnNumber(), None)
        case ('#', data) => (SkippingCommentLine -> data.incrementColumnNumber(), None)
        case (char, data) =>
          val newData = data
            .appendToValue(char)
            .incrementColumnNumber()
          (ConsumingNonQuotedValue -> newData, None)
      }

      override def finalize(data: Data): Option[LineValues] = None
    }

    case object ConsumingNonQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case (' ', data) =>
          val newData = data
            .commitValue()
            .incrementColumnNumber()
          (SkippingSpace -> newData, None)
        case ('\n', data) =>
          val (newData, lineValues) = data
            .commitValue()
            .commitLineValues()
          (ExpectingLine -> newData.incrementLineNumber(), Some(lineValues))
        case ('"', data) =>
          val newData = data
            .appendToValue('"')
            .incrementColumnNumber()
          (ConsumedQuoteInNonQuotedValue -> newData, None)
        case (char, data) =>
          val newData = data
            .appendToValue(char)
            .incrementColumnNumber()
          (this -> newData, None)
      }

      override def finalize(data: Data): Option[LineValues] = {
        val (_, lineValues) = data
          .commitValue()
          .commitLineValues()
        Some(lineValues)
      }
    }

    case object ConsumingQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case ('\n', data) =>
          val newData = data
            .appendToValue('\n')
            .incrementLineNumber()
          (this -> newData, None)
        case ('"', data) => (ConsumedQuoteInQuotedValue -> data.incrementColumnNumber(), None)
        case (char, data) =>
          val newData = data
            .appendToValue(char)
            .incrementColumnNumber()
          (this -> newData, None)
      }

      override def finalize(data: Data): Option[LineValues] = throw new InterruptedContentException
    }

    case object ConsumedQuoteInQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case (' ', data) =>
          val newData = data
            .commitValue()
            .incrementColumnNumber()
          (SkippingSpace -> newData, None)
        case ('\n', data) =>
          val (newData, lineValues) = data
            .commitValue()
            .commitLineValues()
          (ExpectingLine -> newData.incrementLineNumber(), Some(lineValues))
        case ('"', data) =>
          val newData = data
            .appendToValue('"')
            .incrementColumnNumber()
          (ConsumingQuotedValue -> newData, None)
      }

      override def finalize(data: Data): Option[LineValues] = {
        val (_, lineValues) = data
          .commitValue()
          .commitLineValues()
        Some(lineValues)
      }
    }

    case object ConsumedQuoteInNonQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case ('"', data) => (ConsumingNonQuotedValue -> data.incrementColumnNumber(), None)
      }

      override def finalize(data: Data): Option[LineValues] = throw new InterruptedContentException
    }

    case object SkippingSpace extends State {
      override def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case (' ', data) => (this -> data.incrementColumnNumber(), None)
        case ('\n', data) =>
          val (newData, lineValues) = data.commitLineValues()
          (ExpectingLine -> newData.incrementLineNumber(), Some(lineValues))
        case ('"', data) => (ConsumingQuotedValue -> data.incrementColumnNumber(), None)
        case (char, data) =>
          val newData = data
            .appendToValue(char)
            .incrementColumnNumber()
          (ConsumingNonQuotedValue -> newData, None)
      }

      override def finalize(data: Data): Option[LineValues] = {
        val (_, lineValues) = data.commitLineValues()
        Some(lineValues)
      }
    }

    case object SkippingCommentLine extends State {
      override def consume: PartialFunction[(Char, Data), ((State, Data), Option[LineValues])] = {
        case ('\n', data) => (ExpectingLine -> data.incrementLineNumber(), None)
        case (_, data) => (this -> data.incrementColumnNumber(), None)
      }

      override def finalize(data: Data): Option[LineValues] = None
    }

    val initialState: State = ExpectingLine
    val initialData = Data(Vector.empty, "", 1, 1)

    chars.traverse(initialState -> initialData) { case (state -> data, char) =>
      state.consume(data)(char)
    } { case (finalState, finalData) =>
      finalState.finalize(finalData)
    }
  }

  def writeTo(outputStream: OutputStream)(lines: Iterator[LineValues]): Unit = {
    val QuotationMark = '"'
    use(new PrintWriter(outputStream)) { writer =>
      for (lineValues <- lines) {
        writer.println(
          lineValues
            .zipWithIndex
            .map { case (value, i) =>
              val valueWithEscapedQuotationMarks = value.replace(s"$QuotationMark", s"$QuotationMark$QuotationMark")
              if (valueWithEscapedQuotationMarks.contains(' ') || (i == 0 && valueWithEscapedQuotationMarks.startsWith("#"))) {
                s"$QuotationMark$valueWithEscapedQuotationMarks$QuotationMark"
              } else {
                valueWithEscapedQuotationMarks
              }
            }
            .mkString("  ")
        )
      }
    }
  }

  def matchValues[A](lineValues: Seq[String])(pf: PartialFunction[Seq[String], A]): A =
    pf.apply(lineValues)
}
