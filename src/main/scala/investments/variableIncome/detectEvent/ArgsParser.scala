package sisgrana
package investments.variableIncome.detectEvent

import investments.variableIncome.ArgumentsParser
import java.io.PrintStream
import scala.util.{Failure, Success, Try}

object ArgsParser extends ArgumentsParser[(String, Double, List[String])] {
  override protected val spec: Parser[(String, Double, List[String])] =
    for {
      asset <- asset
      minVariation <- minVariation
      paths <- paths
    } yield (asset, minVariation, paths)

  private def asset: Parser[String] = takeNext

  private def minVariation: Parser[Double] =
    for {
      minVariationString <- takeNext
      minVariation =
        Try {
          minVariationString match {
            case s"${str}%" => str.toDouble / 100.0
            case str => str.toDouble
          }
        } match {
          case Failure(exception) => error(s"Valor inválido: $minVariationString", exception)
          case Success(minVariation) => minVariation
        }
    } yield minVariation

  private def paths: Parser[List[String]] =
    for {
      h <- takeNext
      t <- takeRemaining
    } yield h :: t

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados: ATIVO VARIAÇÃO-MÍNIMA ARQUIVO-DE-COTAÇÕES.ZIP...")
    printStream.println()
    printStream.println("VARIAÇÃO-MÍNIMA pode ser em formato percentual.")
    printStream.println("""Exemplos: "12.3%" ou "0.123"""")
  }
}
