package sisgrana
package utils

import java.io.PrintStream

object Exit {
  def apply(): Nothing =
    exitWithStatus(0)

  def withErrorMessage(f: PrintStream => Unit): Nothing = {
    f(Console.err)
    exitWithStatus(1)
  }

  private def exitWithStatus(status: Int): Nothing =
    System.exit(status).asInstanceOf[Nothing]
}
