package sisgrana
package investments.variableIncome.importAssets

import com.softwaremill.quicklens._
import java.io.{File, PrintWriter}
import scala.io.Source
import utils.use

object SSV {
  case class UnexpectedCharacterException(char: Char, lineNumber: Int, columnNumber: Int) extends Exception {
    override def getMessage: String =
      s"Unexpected character '$char' (\\u${char.toHexString}) at $lineNumber:$columnNumber"
  }

  class InterruptedContentException extends Exception

  def readFile(file: File): Seq[Seq[String]] = {
    try {
      use(Source.fromFile(file)) { source =>
        fromChars(source)
      }
    } catch {
      case e: Throwable => throw new Exception(s"Exception while processing file ${file.getName}", e)
    }
  }

  private[investments] def fromChars(chars: Iterator[Char]): Seq[Seq[String]] = {
    case class Data(linesValues: Vector[Vector[String]], lineValues: Vector[String], value: String, lineNumber: Int, columnNumber: Int) {
      def appendToValue(char: Char): Data =
        this.modify(_.value).using(_ :+ char)

      def commitValue(): Data =
        this
          .modify(_.lineValues).using(_ :+ this.value)
          .modify(_.value).setTo("")

      def commitLineValues(): Data =
        this
          .modify(_.linesValues).using(_ :+ this.lineValues)
          .modify(_.lineValues).setTo(Vector.empty)

      def incrementLineNumber(): Data =
        this
          .modify(_.lineNumber).using(_ + 1)
          .modify(_.columnNumber).setTo(1)

      def incrementColumnNumber(): Data =
        this.modify(_.columnNumber).using(_ + 1)
    }

    sealed trait State {
      def consume: PartialFunction[(Char, Data), (State, Data)]

      final def consume(data: Data)(char: Char): (State, Data) =
        consume.applyOrElse((char, data), { _: (Char, Data) =>
          throw UnexpectedCharacterException(char, data.lineNumber, data.columnNumber)
        })

      def finalize(data: Data): Data
    }

    case object ExpectingLine extends State {
      override val consume: PartialFunction[(Char, Data), (State, Data)] = {
        case (' ', data) => SkippingSpace -> data.incrementColumnNumber()
        case ('\n', data) =>
          this -> data
            .commitLineValues()
            .incrementLineNumber()
        case ('"', data) => ConsumingQuotedValue -> data.incrementColumnNumber()
        case ('#', data) => SkippingCommentLine -> data.incrementColumnNumber()
        case (char, data) =>
          ConsumingNonQuotedValue -> data
            .appendToValue(char)
            .incrementColumnNumber()
      }

      override def finalize(data: Data): Data = data
    }

    case object ConsumingNonQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), (State, Data)] = {
        case (' ', data) =>
          SkippingSpace -> data
            .commitValue()
            .incrementColumnNumber()
        case ('\n', data) =>
          ExpectingLine -> data
            .commitValue()
            .commitLineValues()
            .incrementLineNumber()
        case ('"', data) =>
          ConsumedQuoteInNonQuotedValue -> data
            .appendToValue('"')
            .incrementColumnNumber()
        case (char, data) =>
          this -> data
            .appendToValue(char)
            .incrementColumnNumber()
      }

      override def finalize(data: Data): Data =
        data
          .commitValue()
          .commitLineValues()
    }

    case object ConsumingQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), (State, Data)] = {
        case ('\n', data) =>
          this -> data
            .appendToValue('\n')
            .incrementLineNumber()
        case ('"', data) => ConsumedQuoteInQuotedValue -> data.incrementColumnNumber()
        case (char, data) =>
          this -> data
            .appendToValue(char)
            .incrementColumnNumber()
      }

      override def finalize(data: Data): Data = throw new InterruptedContentException
    }

    case object ConsumedQuoteInQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), (State, Data)] = {
        case (' ', data) =>
          SkippingSpace -> data
            .commitValue()
            .incrementColumnNumber()
        case ('\n', data) =>
          ExpectingLine -> data
            .commitValue()
            .commitLineValues()
            .incrementLineNumber()
        case ('"', data) =>
          ConsumingQuotedValue -> data
            .appendToValue('"')
            .incrementColumnNumber()
      }

      override def finalize(data: Data): Data =
        data
          .commitValue()
          .commitLineValues()
    }

    case object ConsumedQuoteInNonQuotedValue extends State {
      override def consume: PartialFunction[(Char, Data), (State, Data)] = {
        case ('"', data) => ConsumingNonQuotedValue -> data.incrementColumnNumber()
      }

      override def finalize(data: Data): Data = throw new InterruptedContentException
    }

    case object SkippingSpace extends State {
      override def consume: PartialFunction[(Char, Data), (State, Data)] = {
        case (' ', data) => this ->  data.incrementColumnNumber()
        case ('\n', data) =>
          ExpectingLine -> data
            .commitLineValues()
            .incrementLineNumber()
        case ('"', data) => ConsumingQuotedValue -> data.incrementColumnNumber()
        case (char, data) =>
          ConsumingNonQuotedValue -> data
            .appendToValue(char)
            .incrementColumnNumber()
      }

      override def finalize(data: Data): Data = data.commitLineValues()
    }

    case object SkippingCommentLine extends State {
      override def consume: PartialFunction[(Char, Data), (State, Data)] = {
        case ('\n', data) => ExpectingLine -> data.incrementLineNumber()
        case (_, data) => this -> data.incrementColumnNumber()
      }

      override def finalize(data: Data): Data = data
    }

    val initialState = ExpectingLine: State
    val initialData = Data(Vector.empty, Vector.empty, "", 1, 1)
    val (finalState, finalData) = chars.foldLeft(initialState -> initialData) { case ((state, data), char) =>
      state.consume(data)(char)
    }
    finalState
      .finalize(finalData)
      .linesValues
  }

  def writeFile(file: File)(lines: Iterable[Seq[String]]): Unit = {
    val QuotationMark = '"'
    use(new PrintWriter(file)) { writer =>
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
