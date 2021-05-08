package sisgrana
package investments.irpf

import scala.language.implicitConversions

class IndentedPrinter {
  import IndentedPrinter.Node

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

  def node(x: Any)(children: Node*): Node =
    Node(x, children)

  def printHierarchy(node: Node): Unit =
    node.singleLine match {
      case Some(line) => println(line)
      case None => context(node.x) {
        for (child <- node.children) {
          printHierarchy(child)
        }
      }
    }
}

object IndentedPrinter {
  case class Node(x: Any, children: Seq[Node]) {
    def singleLine: Option[String] =
      children match {
        case Seq() => Some(s"$x")
        case Seq(child) => child.singleLine.map(childLine => s"$x $childLine")
        case _ => None
      }
  }

  implicit def toNode(x: Any): Node = new Node(x, Seq.empty)
}
