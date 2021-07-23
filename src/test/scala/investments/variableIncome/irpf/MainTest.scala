package sisgrana
package investments.variableIncome.irpf

import investments.variableIncome.irpf.Main.DoubleOps
import investments.variableIncome.model.AmountWithCost
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class MainTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("Main.formatPositionChangeRaw()") {
    val cases = Table(
      (
        "from",
        "change",
        "to",
        "expectedFormat",
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(0, 0, 0),
        AmountWithCost.fromSignedQuantityAndAverages(0, 0, 0),
        AmountWithCost.fromSignedQuantityAndAverages(0, 0, 0),
        "0 + 0 = 0",
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(0, 0, 0),
        AmountWithCost.fromSignedQuantityAndAverages(+10, 2.00, 0.02),
        AmountWithCost.fromSignedQuantityAndAverages(+10, 2.00, 0.02),
        s"0 + (10 x ${2.02.format} = ${20.2.format}) = (10 x ${2.02.format} = ${20.20.format})",
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(0, 0, 0),
        AmountWithCost.fromSignedQuantityAndAverages(-10, 2.00, 0.02),
        AmountWithCost.fromSignedQuantityAndAverages(-10, 2.00, 0.02),
        s"0 - (10 x ${1.98.format} = ${19.80.format}) = (-10 x ${1.98.format} = ${(-19.80).format})",
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(+10, 1.50, 0.15),
        AmountWithCost.fromSignedQuantityAndAverages(+5, 3.00, 0.30),
        AmountWithCost.fromSignedQuantityAndAverages(+15, 2.00, 0.20),
        s"(10 x ${1.65.format} = ${16.50.format}) + (5 x ${3.30.format} = ${16.50.format}) = (15 x ${2.20.format} = ${33.00.format})"
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(+10, 1.50, 0.15),
        AmountWithCost.fromSignedQuantityAndAverages(-5, 3.00, 0.30),
        AmountWithCost.fromSignedQuantityAndAverages(+5, 1.50, 0.15),
        s"(10 x ${1.65.format} = ${16.50.format}) - 5 unidades = (5 x ${1.65.format} = ${8.25.format})"
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(+10, 1.50, 0.15),
        AmountWithCost.fromSignedQuantityAndAverages(-10, 3.00, 0.30),
        AmountWithCost.fromSignedQuantityAndAverages(0, 0.00, 0.00),
        s"(10 x ${1.65.format} = ${16.50.format}) - 10 unidades = 0"
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(-10, 1.50, 0.15),
        AmountWithCost.fromSignedQuantityAndAverages(-5, 3.00, 0.30),
        AmountWithCost.fromSignedQuantityAndAverages(-15, 2.00, 0.20),
        s"(-10 x ${1.35.format} = ${(-13.50).format}) - (5 x ${2.70.format} = ${13.50.format}) = (-15 x ${1.80.format} = ${(-27.00).format})"
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(-10, 1.50, 0.15),
        AmountWithCost.fromSignedQuantityAndAverages(+5, 3.00, 0.30),
        AmountWithCost.fromSignedQuantityAndAverages(-5, 1.50, 0.15),
        s"(-10 x ${1.35.format} = ${(-13.50).format}) + 5 unidades = (-5 x ${1.35.format} = ${(-6.75).format})"
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(-10, 1.50, 0.15),
        AmountWithCost.fromSignedQuantityAndAverages(+10, 3.00, 0.30),
        AmountWithCost.fromSignedQuantityAndAverages(0, 0.00, 0.00),
        s"(-10 x ${1.35.format} = ${(-13.50).format}) + 10 unidades = 0"
      ),
    )

    forAll(cases) { case (from, change, to, expectedFormat) =>
      val format = Main.formatPositionChangeRaw(from, change, to)
      format should equal (expectedFormat)
    }
  }
}
