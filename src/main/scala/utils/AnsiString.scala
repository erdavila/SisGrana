package sisgrana
package utils

import scala.collection.mutable
import scala.language.implicitConversions

class AnsiString private(private val elements: Vector[AnsiString.Element]) {
  def ++ (other: AnsiString): AnsiString =
    new AnsiString(this.elements ++ other.elements)

  def length: Int =
    elements
      .collect { case AnsiString.StringElement(string) => string.length }
      .sum

  override def toString: String = {
    val buffer = new mutable.StringBuilder

    def escape(codes: Vector[Int]): String =
      if (codes.nonEmpty) {
        codes.mkString("\u001b[", ";", "m")
      } else {
        ""
      }

    val finalCodes = elements.foldLeft(Vector.empty[Int]) { (codes, block) =>
      block match {
        case AnsiString.StringElement(string) =>
          buffer.append(escape(codes))
          buffer.append(string)
          Vector.empty

        case AnsiString.CodeElement(code) => codes :+ code
      }
    }

    buffer.append(escape(finalCodes))
    buffer.toString()
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[AnsiString]

  override def equals(other: Any): Boolean = other match {
    case that: AnsiString =>
      (that canEqual this) &&
        elements == that.elements
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(elements)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object AnsiString {
  private sealed trait Element
  private case class StringElement(string: String) extends Element
  private case class CodeElement(code: Int) extends Element

  trait RangeFormat extends (AnsiString => AnsiString) { self =>
    final def apply(ansiString: AnsiString): AnsiString =
      rangePrefix ++ ansiString ++ rangeSuffix

    def rangePrefix: AnsiString
    def rangeSuffix: AnsiString

    @inline def <|> (other: RangeFormat): RangeFormat = compose(other)
    final def compose(other: RangeFormat): RangeFormat = new RangeFormat {
      override def rangePrefix: AnsiString = self.rangePrefix ++ other.rangePrefix
      override def rangeSuffix: AnsiString = other.rangeSuffix ++ self.rangeSuffix
    }
  }

  trait Format {
    def code: Int

    def ++ (ansiString: AnsiString): AnsiString =
      AnsiString.fromFormat(this) ++ ansiString
  }

  object Format {
    val Reset: Format = SimpleFormat(0)
    val NormalIntensity: Format = SimpleFormat(22)

    val Bold: SimpleRangeFormat = BoldFormat
    val DefaultColor: DefaultColorFormat = DefaultColorFormat()

    val Red: ColorFormat = ColorFormat(31)
    val Green: ColorFormat = ColorFormat(32)
    val Yellow: ColorFormat = ColorFormat(33)
    val Blue: ColorFormat = ColorFormat(34)
    val Magenta: ColorFormat = ColorFormat(35)
    val Cyan: ColorFormat = ColorFormat(36)
    val White: ColorFormat = ColorFormat(37)
  }

  case class SimpleFormat(code: Int) extends Format

  abstract class SimpleRangeFormat(val code: Int, endFormat: Format) extends Format with RangeFormat {
    override def rangePrefix: AnsiString = AnsiString.fromFormat(SimpleFormat(code))
    override def rangeSuffix: AnsiString = AnsiString.fromFormat(endFormat)
  }

  case object BoldFormat extends SimpleRangeFormat(code = 1, endFormat = Format.NormalIntensity)

  case class ColorFormat(
    baseCode: Int,
    isBackground: Boolean = false,
    isBright: Boolean = false,
  ) extends Format with RangeFormat {
    override def code: Int =  baseCode + (if (isBackground) 10 else 0) + (if (isBright) 60 else 0)

    @inline def fg: ColorFormat = foreground
    def foreground: ColorFormat = this.copy(isBackground = false)

    @inline def bg: ColorFormat = background
    def background: ColorFormat = this.copy(isBackground = true)

    def bright: ColorFormat = this.copy(isBright = true)
    def notBright: ColorFormat = this.copy(isBright = false)

    override def rangePrefix: AnsiString = AnsiString.fromFormat(SimpleFormat(code))
    override def rangeSuffix: AnsiString = AnsiString.fromFormat(if (isBackground) Format.DefaultColor.bg else Format.DefaultColor)
  }

  case class DefaultColorFormat(isBackground: Boolean = false) extends Format {
    override def code: Int = 39 + (if (isBackground) 10 else 0)

    @inline def fg: DefaultColorFormat = foreground
    def foreground: DefaultColorFormat = this.copy(isBackground = false)

    @inline def bg: DefaultColorFormat = background
    def background: DefaultColorFormat = this.copy(isBackground = true)
  }

  implicit def fromString(string: String): AnsiString =
    new AnsiString(Vector(StringElement(string)))

  implicit def fromFormat(format: Format): AnsiString =
    new AnsiString(Vector(CodeElement(format.code)))

  implicit class StringOps(private val string: String) extends AnyVal {
    def ++ (ansiString: AnsiString): AnsiString =
      fromString(string) ++ ansiString

    def ++ (format: Format): AnsiString =
      fromString(string) ++ fromFormat(format)
  }
}
