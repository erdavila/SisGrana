package sisgrana
package investments.irpf

class IndentedPrinter {
  private val Indentation = "  "
  private var indent: Int = 0

  def context[A](x: Any)(block: => A): A = {
    println(x)
    indent += 1
    val result =
      try block
      finally {
        indent -= 1
      }
    result
  }

  def println(x: Any): Unit =
    Predef.println(s"${Indentation * indent}$x")

  def println(): Unit =
    Predef.println()
}
