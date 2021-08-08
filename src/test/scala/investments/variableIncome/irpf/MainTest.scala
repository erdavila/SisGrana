package sisgrana
package investments.variableIncome.irpf

import investments.variableIncome.irpf.Main.DoubleOps
import investments.variableIncome.model.Amount

class MainTest extends TestBase {
  test("formatPositionChange()") {
    val cases = Table(
      (
        "posBefore",
        "posAfter",
        "expected output",
      ),
      (
        Amount.fromSignedQuantityAndAverages(+10, 1.50, 0.15),
        Amount.fromSignedQuantityAndAverages(+15, 2.00, 0.20),
        s"(10 x ${1.65.format} = ${16.50.format}) + (5 x ${3.30.format} = ${16.50.format}) = (15 x ${2.20.format} = ${33.00.format})"
      ),
      (
        Amount.fromSignedQuantityAndAverages(+10, 1.50, 0.15),
        Amount.fromSignedQuantityAndAverages(+5, 1.50, 0.15),
        s"(10 x ${1.65.format} = ${16.50.format}) - 5 unidades = (5 x ${1.65.format} = ${8.25.format})"
      ),
      (
        Amount.fromSignedQuantityAndAverages(-10, 1.50, 0.15),
        Amount.fromSignedQuantityAndAverages(-15, 2.00, 0.20),
        s"(-10 x ${1.35.format} = ${(-13.50).format}) - (5 x ${2.70.format} = ${13.50.format}) = (-15 x ${1.80.format} = ${(-27.00).format})"
      ),
      (
        Amount.fromSignedQuantityAndAverages(-10, 1.50, 0.15),
        Amount.fromSignedQuantityAndAverages(-5, 1.50, 0.15),
        s"(-10 x ${1.35.format} = ${(-13.50).format}) + 5 unidades = (-5 x ${1.35.format} = ${(-6.75).format})"
      ),
      (
        Amount.fromSignedQuantityAndAverages(-5, 1.50, 0.15),
        Amount.fromSignedQuantityAndAverages(+5, 2.00, 0.20),
        s"(-5 x ${1.35.format} = ${(-6.75).format}) + 10 unidades = (5 x ${2.2.format} = ${11.0.format})"
      ),
      (
        Amount.fromSignedQuantityAndAverages(+5, 1.50, 0.15),
        Amount.fromSignedQuantityAndAverages(-5, 2.00, 0.20),
        s"(5 x ${1.65.format} = ${8.25.format}) - 10 unidades = (-5 x ${1.8.format} = ${(-9).format})"
      ),
    )

    forAll(cases) { case (posBefore, posAfter, expectedOutput) =>
      val output = Main.formatPositionChange(posBefore, posAfter, withLabel = false)

      output should equal (expectedOutput)
    }
  }
}
