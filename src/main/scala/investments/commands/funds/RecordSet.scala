package sisgrana
package investments.commands.funds

import investments.Rate
import java.time.LocalDate

case class RecordSet(
  position: RecordSet.Position,
  accumulated: RecordSet.Accumulated,
) {
  lazy val records: Map[String, Record] =
    accumulated.records.map { case (fund, accumulatedRecord) =>
      val positionRecord = position.positionRecords.get(fund)
      val record = Record(positionRecord, accumulatedRecord)
      fund -> record
    }
}

object RecordSet {
  case class Position(
    date: LocalDate,
    days: Int,
    positionRecords: Map[String, Presence[Record.Position]],
    missingData: Boolean,

    // Totals
    totalYieldRate: Option[Rate],
    totalYieldResult: Option[Double],
    totalInitialBalance: Option[Double],
    totalBalanceChange: Option[Double],
    totalFinalBalance: Option[Double],
  ) extends RecordSet.Position.Previous

  object Position {
    case class Initial(
      date: LocalDate,
      positionRecords: Map[String, Present[Record.Position.Initial]],
      totalFinalBalance: Option[Double],
    ) extends RecordSet.Position.Previous {
      override def missingData: Boolean = false
    }

    sealed trait Previous {
      def date: LocalDate
      def positionRecords: Map[String, Presence[Record.Position.Previous]]
      def missingData: Boolean
      def totalFinalBalance: Option[Double]
    }
  }

  case class Accumulated(
    days: Int,
    records: Map[String, Record.Accumulated],
    totalYieldRate: Option[Rate],
    totalYieldResult: Option[Double],
    totalBalanceChange: Option[Double],
    missingData: Boolean,
  )
}
