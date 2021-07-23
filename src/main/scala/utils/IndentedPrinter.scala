package sisgrana
package utils

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

  def context[A](x: Any, enabled: Boolean)(block: => A): A =
    if (enabled) {
      context(x)(block)
    } else {
      block
    }

  def println(x: Any): Unit =
    Predef.println(s"${Indentation * indent}$x")

  def println(): Unit =
    Predef.println()

  object hierarchy {
    def leaf(x: Any): Node =
      Node(x, Seq.empty)

    def tree(x: Any)(children: Node*): Node =
      Node(x, children)

    def optionalTree(x: Any)(children: Option[Node]*): Option[Node] = {
      val childNodes = children.map(_.toSeq).reduce(_ ++ _)
      Option.when(childNodes.nonEmpty) {
        tree(x)(childNodes: _*)
      }
    }

    def optionalTree(x: Any, enabled: Boolean)(children: Option[Node]*): Option[Node] =
      if (enabled) {
        optionalTree(x)(children: _*)
      } else {
        None
      }

    def print(node: Node): Unit =
      node.singleLine match {
        case Some(line) => println(line)
        case None => context(node.x) {
          for (child <- node.children) {
            print(child)
          }
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
