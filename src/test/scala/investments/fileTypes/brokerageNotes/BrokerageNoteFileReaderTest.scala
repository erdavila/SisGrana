package sisgrana
package investments.fileTypes.brokerageNotes

import investments.commands.multiImport.eventsAndBrokerageNotes.NameNormalizer
import investments.files.SSV
import java.time.LocalDate

class BrokerageNoteFileReaderTest extends TestBase {
  private val Stockbroker = "stockbroker"
  private val Date = LocalDate.now()

  private val nameNormalizer = new NameNormalizer(
    Map(
      "The X" -> "XXXX",
      "YYYY" -> "YYYY",
      "ZZZZ" -> "ZZZZ",
    )
  )

  private def brokerageNotesFromLineValues(linesValues: Seq[SSV.LineValues]) =
    BrokerageNoteFileReader.fromLinesValues(Date, Stockbroker, nameNormalizer)(linesValues.iterator).toSeq

  test("Valid cases for .fromLinesValues()") {
    val cases = Table(
      (
        "linesValues",
        "expected brokerageNotes",
      ),
      (
        Seq(
          Seq("C", "The X", "3", "12,34"),
          Seq("V", "YYYY", "4", "43,21"),
          Seq("EC", "AAAAA123", "ZZZZ", "5", "9,99"),
          Seq.empty,
          Seq("cost 1", "0,10"),
          Seq("cost 2", "0,05"),
          Seq.empty,
          Seq("85,72"),
        ),
        Seq(
          BrokerageNote(
            Stockbroker, Date,
            List(
              Negotiation(Operation.Purchase, "XXXX", 3, 12.34, None),
              Negotiation(Operation.Sale, "YYYY", 4, 43.21, None),
              Negotiation(Operation.Purchase, "ZZZZ", 5, 9.99, Some("AAAAA123")),
            ),
            List(
              Cost("cost 1", 0.10),
              Cost("cost 2", 0.05),
            ),
            totalValue = 85.72,
          )
        ),
      ),
      // Multiple brokerage notes
      (
        Seq(
          Seq("C", "The X", "1", "23,45"),
          Seq.empty,
          Seq("cost", "0,10"),
          Seq.empty,
          Seq("-23,55"),
          Seq.empty,
          Seq("V", "YYYY", "6", "7,89"),
          Seq.empty,
          Seq("cost", "0,87"),
          Seq.empty,
          Seq("46,47"),
        ),
        Seq(
          BrokerageNote(
            Stockbroker, Date,
            List(Negotiation(Operation.Purchase, "XXXX", 1, 23.45, None)),
            List(Cost("cost", 0.10)),
            totalValue = -23.55,
          ),
          BrokerageNote(
            Stockbroker, Date,
            List(Negotiation(Operation.Sale, "YYYY", 6, 7.89, None)),
            List(Cost("cost", 0.87)),
            totalValue = 46.47,
          ),
        ),
      ),
    )

    forAll(cases) { case (linesValues, expectedBrokerageNotes) =>
      val brokerageNotes = brokerageNotesFromLineValues(linesValues)

      brokerageNotes should equal (expectedBrokerageNotes)
    }
  }

  test("Invalid total in .fromLinesValues()") {
    val e = the [Exception] thrownBy brokerageNotesFromLineValues(
      Seq(
        Seq("C", "The X", "1", "23,45"),
        Seq.empty,
        Seq("cost", "0,10"),
        Seq.empty,
        Seq("total", "-23,55"),
      )
    )

    e.getMessage should startWith ("Invalid values for total")
  }

  private val CompleteSimpleLinesValues = Seq(
    Seq("C", "The X", "1", "23,45"),
    Seq.empty,
    Seq("cost", "0,10"),
    Seq.empty,
    Seq("-23,55"),
  )

  test("Missing data in .fromLinesValues()") {
    for (numLines <- 1 until CompleteSimpleLinesValues.length) {
      withClue(s"[$numLines lines]") {
        val linesValues = CompleteSimpleLinesValues.take(numLines)
        val e = the [Exception] thrownBy brokerageNotesFromLineValues(linesValues)

        e.getMessage should equal ("Missing data")
      }
    }
  }

  test("Exceeding data in .fromLinesValues()") {
    val linesValues = CompleteSimpleLinesValues :+ Seq("excess")
    val e = the [Exception] thrownBy brokerageNotesFromLineValues(linesValues)

    e.getMessage should startWith ("Exceeding data")
  }
}
