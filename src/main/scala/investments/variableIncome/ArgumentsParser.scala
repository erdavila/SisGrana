package sisgrana
package investments.variableIncome

import cats.data.StateT
import java.io.PrintStream
import scala.util.{Failure, Success, Try}

trait ArgumentsParser[A] {
  protected type Args = List[String]

  protected type Parser[T] = StateT[Try, Args, T]

  protected val Delimiter = "--"

  protected def spec: Parser[A]

  protected def printUsage(printStream: PrintStream): Unit

  def parse(args: Array[String]): A = {
    if (args.iterator.takeWhile(_ != Delimiter).exists(arg => arg == "-h" || arg == "--help")) {
      printUsage(Console.out)
      exit(0)
    }

    val result = for {
      result <- spec
      remaining <- takeRemaining
      _ = remaining.headOption.foreach(h => error(s"Parâmetro inesperado: $h"))
    } yield result


    result.runA(args.toList) match {
      case Success(result) => result
      case Failure(exception) =>
        Console.err.println(exception.getMessage)
        Console.err.println()
        printUsage(Console.err)
        exit(1)
    }
  }

  protected def takeNext: Parser[String] =
    StateT {
      case h :: t => Success((t, h))
      case Nil => Failure(new Exception("Argumento faltando"))
    }

  protected def takeRemaining: Parser[List[String]] =
    StateT { args => success((List.empty, args)) }

  protected def error(str: String): Nothing =
    throw new Exception(str)

  protected def error(str: String, exception: Throwable): Nothing =
    throw new Exception(str, exception)

  private def success[T](t: T): Try[T] = Success(t)

  private def exit(status: Int): Nothing =
    System.exit(status).asInstanceOf[Nothing]
}
