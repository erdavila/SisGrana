package sisgrana
package investments.variableIncome.importQuotes

import java.io.File

case class MultiFile(components: Vector[File]) {
  require(components.nonEmpty)

  def /(path: String): MultiFile = this / new File(path)
  def /(file: File): MultiFile = MultiFile(components :+ file)

  def name: String = components.last.getName

  override def toString: String = components.mkString("//")
}

object MultiFile {
  def apply(path: String): MultiFile = MultiFile(new File(path))
  def apply(file: File): MultiFile = MultiFile(Vector(file))
}
