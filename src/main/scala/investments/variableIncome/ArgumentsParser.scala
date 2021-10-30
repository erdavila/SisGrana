package sisgrana
package investments.variableIncome

import cats.data.StateT
import investments.variableIncome.files.{FilePathResolver, MultiLevelFilePath}
import java.io.PrintStream
import java.time.LocalDate
import scala.util.{Failure, Success, Try}
import utils.{DateRange, DateRanges}

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

  private val MissingArgument = "Argumento faltando"

  protected def takeNext: Parser[String] =
    StateT {
      case h :: t => Success((t, h))
      case Nil => Failure(new Exception(MissingArgument))
    }

  protected def takeNextIf(condition: String => Boolean): Parser[Option[String]] =
    StateT {
      case h :: t if condition(h) => Success((t, Some(h)))
      case args => Success((args, None))
    }

  protected def discardNext: Parser[Unit] =
    for (_ <- takeNext)
      yield ()

  protected def takeWhile(f: String => Boolean, atLeast: Int = 0): Parser[Args] =
    returnAtLeast(atLeast) { args => args.span(f).swap }

  protected def takeRemaining: Parser[Args] =
    takeRemaining(atLeast = 0)

  protected def takeRemaining(atLeast: Int): Parser[Args] =
    returnAtLeast(atLeast) { args => (Nil, args) }

  private def returnAtLeast(atLeast: Int)(f: Args => (Args, List[String])): Parser[List[String]] =
    StateT { args =>
      val (remaining, toBeReturned) = f(args)
      if (toBeReturned.lengthIs >= atLeast) {
        success((remaining, toBeReturned))
      } else {
        failure(MissingArgument)
      }
    }

  protected implicit class ParserOps[T](parser: Parser[T]) {
    def $ [U](f: T => U): Parser[U] =
      for (t <- parser)
        yield f(t)
  }

  protected def error(str: String): Nothing =
    throw new Exception(str)

  protected def error(str: String, exception: Throwable): Nothing =
    throw new Exception(str, exception)

  private def success[T](t: T): Try[T] = Success(t)

  private def failure(str: String): Try[Nothing] = Failure(new Exception(str))

  private def exit(status: Int): Nothing =
    System.exit(status).asInstanceOf[Nothing]
}

object ArgumentsParser {
  trait Utils { self: ArgumentsParser[_] =>
    protected def toPaths(args: Args): Seq[MultiLevelFilePath] =
      FilePathResolver.resolve(args)

    protected def toDateRanges(args: Args): DateRanges = {
      def parseDate(arg: String): LocalDate =
        try {
          LocalDate.parse(arg)
        } catch {
          case e: Throwable => error(s"Data inválida: ${e.getMessage}", e)
        }

      val ranges =
        for (arg <- args)
          yield {
            arg match {
              case s"$begin:$end" =>
                val beginDate = parseDate(begin)
                val endDate = parseDate(end)
                DateRange(beginDate, endDate)
              case _ =>
                val date = parseDate(arg)
                DateRange(date, date)
            }
          }

      DateRanges.from(ranges)(DateRange.Mode.FullDay)
    }
  }
}
