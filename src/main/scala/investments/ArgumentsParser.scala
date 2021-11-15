package sisgrana
package investments

import cats.data.State
import investments.files.{FilePathResolver, MultiLevelFilePath}
import java.io.PrintStream
import java.time.LocalDate
import utils.{DateRange, DateRanges, Exit}

trait ArgumentsParser[A] {
  protected type Args = List[String]

  protected type Parser[T] = State[Args, T]

  protected val Delimiter = "--"

  protected def spec: Parser[A]

  protected def printUsage(printStream: PrintStream): Unit

  def parse(args: Array[String]): A = {
    if (args.iterator.takeWhile(_ != Delimiter).exists(arg => arg == "-h" || arg == "--help")) {
      printUsage(Console.out)
      Exit()
    }

    val result = for {
      result <- spec
      remaining <- takeRemaining
      _ = remaining.headOption.foreach(h => error(s"Parâmetro inesperado: $h"))
    } yield result


    try {
      result.runA(args.toList).value
    } catch {
      case exception: Throwable =>
        Exit.withErrorMessage { printStream =>
          printStream.println(exception.getMessage)
          printStream.println()
          printUsage(printStream)
        }
    }
  }

  private val MissingArgument = "Argumento faltando"

  protected def takeNext: Parser[String] =
    for (nextOpt <- takeNextIfAny)
      yield nextOpt.getOrElse(error(MissingArgument))

  protected def takeNextIf(condition: String => Boolean): Parser[Option[String]] =
    State {
      case h :: t if condition(h) => (t, Some(h))
      case args => (args, None)
    }

  protected def takeNextIfAny: Parser[Option[String]] =
    State {
      case h :: t => (t, Some(h))
      case Nil => (Nil, None)
    }

  protected def const[T](value: T): Parser[T] =
    State.pure(value)

  protected def discardNext: Parser[Unit] =
    for (_ <- takeNext)
      yield ()

  protected def takeWhile(f: String => Boolean, atLeast: Int = 0): Parser[Args] =
    returnAtLeast(atLeast) { args => args.span(f).swap }

  protected def takeRemaining: Parser[Args] =
    takeRemaining(atLeast = 0)

  protected def takeRemaining(atLeast: Int): Parser[Args] =
    returnAtLeast(atLeast) { args => (Nil, args) }

  protected def takeOptionParameter(forms: String*): Parser[Option[String]] =
    takeOption(forms, 1)(
      found = (option, params) => {
        assert(params.lengthIs <= 1)
        params.headOption match {
          case Some(param) => Some(param)
          case None => error(s"Faltando parâmetro para a opção $option")
        }
      },
      notFound = () => None,
    )

  protected def takeOption(forms: String*): Parser[Boolean] =
    takeOption(forms, 0)(
      found = (_, _) => true,
      notFound = () => false,
    )

  private def takeOption[T](forms: Seq[String], paramsCount: Int)(
    found: (String, List[String]) => T,
    notFound: () => T,
  ): Parser[T] =
    State { args =>
      val (possiblyOptions, nonOptionArgs) = args.span(_ != Delimiter)
      val i = possiblyOptions.indexWhere(arg => forms.contains(arg))
      if (i >= 0) {
        val (argsBeforeOption, rest) = possiblyOptions.splitAt(i)
        val option = rest.head
        val (params, argsAfterOption) = rest.tail.splitAt(paramsCount)
        val remainingArgs = argsBeforeOption ::: argsAfterOption ::: nonOptionArgs
        (remainingArgs, found(option, params))
      } else {
        (args, notFound())
      }
    }

  private def returnAtLeast(atLeast: Int)(f: Args => (Args, List[String])): Parser[List[String]] =
    State { args =>
      val (remaining, toBeReturned) = f(args)
      if (toBeReturned.lengthIs >= atLeast) {
        (remaining, toBeReturned)
      } else {
        error(MissingArgument)
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
}

object ArgumentsParser {
  trait Utils { self: ArgumentsParser[_] =>
    protected def toPaths(args: Args): Seq[MultiLevelFilePath] =
      FilePathResolver.resolve(args)

    protected def toDate(arg: String): LocalDate =
      try {
        LocalDate.parse(arg)
      } catch {
        case e: Throwable => error(s"Data inválida: ${e.getMessage}", e)
      }

    protected def toDateRanges(args: Args): DateRanges = {
      val ranges =
        for (arg <- args)
          yield {
            arg match {
              case s"$begin:$end" =>
                val beginDate = toDate(begin)
                val endDate = toDate(end)
                DateRange(beginDate, endDate)
              case _ =>
                val date = toDate(arg)
                DateRange(date, date)
            }
          }

      DateRanges.from(ranges)(DateRange.Mode.FullDay)
    }
  }
}
