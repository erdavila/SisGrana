package sisgrana
package investments.irpf

trait Debug {
  def context[A](x: => Any)(a: => A): A
  def ifEnabled(x: => Unit): Unit
  def println(): Unit
  def println(x: => Any): Unit
  def printOwnedAssets(ownedAssets: OwnedAssets): Unit
}

class DebugEnabled extends Debug {
  private val indentedPrinter = new IndentedPrinter

  override def context[A](x: => Any)(block: => A): A =
    indentedPrinter.context(x)(block)

  override def ifEnabled(x: => Unit): Unit =
    x

  override def println(): Unit =
    indentedPrinter.println()

  override def println(x: => Any): Unit =
    indentedPrinter.println(x)

  override def printOwnedAssets(ownedAssets: OwnedAssets): Unit =
    context("Owned Assets") {
      for (oa <- ownedAssets.values) {
        println(oa)
      }
    }
}

class DebugDisabled extends Debug {
  override def context[A](x: => Any)(a: => A): A = a
  override def ifEnabled(x: => Unit): Unit = ()
  override def println(): Unit = ()
  override def println(x: => Any): Unit = ()
  override def printOwnedAssets(ownedAssets: OwnedAssets): Unit = ()
}
