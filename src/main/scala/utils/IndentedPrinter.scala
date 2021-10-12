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

  object hierarchy extends IndentedPrinter.Hierarchy {
    def print(node: Node): Unit =
      node.singleLine match {
        case Some(line) => println(line)
        case None => context(node.str) {
          for (child <- node.children) {
            print(child)
          }
        }
      }
  }
}

object IndentedPrinter {
  case class Node(str: String, children: Seq[Node]) {
    def singleLine: Option[String] =
      children match {
        case Seq() => Some(s"$str")
        case Seq(child) => child.singleLine.map(childLine => s"$str $childLine")
        case _ => None
      }
  }

  implicit def toNode(str: String): Node = Node(str, Seq.empty)

  trait Hierarchy {
    def leaf(str: String): Node =
      Node(str, Seq.empty)

    def tree(str: String)(children: Node*): Node =
      Node(str, children)

    def optionalTree(str: String)(children: Option[Node]*): Option[Node] = {
      val childNodes = children.map(_.toSeq).reduce(_ ++ _)
      Option.when(childNodes.nonEmpty) {
        tree(str)(childNodes: _*)
      }
    }

    def optionalTree(str: String, enabled: Boolean)(children: Option[Node]*): Option[Node] = {
      if (enabled) {
        optionalTree(str)(children: _*)
      } else {
        None
      }
    }
  }

  val Hierarchy: Hierarchy = new Hierarchy {}
}
