package sisgrana
package utils

import scala.language.implicitConversions

class AnsiString private(private val blocks: Array[AnsiString.Block]) {
  def ++(other: AnsiString): AnsiString =
    new AnsiString(this.blocks ++ other.blocks)

  def ++(code: AnsiString.Code): AnsiString =
    this ++ AnsiString.escape(code)

  def length: Int =
    blocks
      .collect { case AnsiString.Content(string) => string.length }
      .sum

  override def toString: String = blocks
    .map {
      case AnsiString.Content(string) => string
      case AnsiString.Escaped(string) => string
    }
    .mkString
}

object AnsiString {
  private sealed trait Block
  private case class Content(string: String) extends Block
  private case class Escaped(string: String) extends Block

  def content(string: String): AnsiString = Content(string)
  def escaped(string: String): AnsiString = Escaped(string)

  def escape(codes: Code*): AnsiString =
    escaped(codes.map(_.value.toString).mkString("\u001b[", ";", "m"))

  case class Code(value: Int) {
    def ++ (string: String): AnsiString =
      escape(this) ++ string
  }
  object Code {
    val Reset: Code = Code(0)
    val Bold: Code = Code(1)
    val NormalIntensity: Code = Code(22)
    val Red: Code = Code(31)
    val Green: Code = Code(32)
    val Yellow: Code = Code(33)
    val Blue: Code = Code(34)
    val Magenta: Code = Code(35)
    val Cyan: Code = Code(36)
    val DefaultColor: Code = Code(39)
    val DefaultBgColor: Code = Code(49)
    def BG(colorCode: Code): Code = Code(colorCode.value + 10)
  }

  implicit def fromString(string: String): AnsiString = content(string)

  implicit class StringOps(private val string: String) extends AnyVal {
    def ++ (ansiString: AnsiString): AnsiString = fromString(string) ++ ansiString
    def ++ (code: Code): AnsiString = fromString(string) ++ escape(code)
  }

  private implicit def fromBlock(block: Block): AnsiString = new AnsiString(Array(block))
}
