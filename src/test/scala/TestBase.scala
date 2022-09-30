package sisgrana

import investments.model.StockbrokerAsset
import utils.CustomMatchers
import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

trait TestBase extends AnyFunSuite with TableDrivenPropertyChecks with Matchers with CustomMatchers with OptionValues

object TestBase {
  val DefaultAsset = "default-asset"
  val DefaultStockbroker = "default-stockbroker"
  val DefaultStockbrokerAsset: StockbrokerAsset = StockbrokerAsset(DefaultStockbroker, DefaultAsset)
}
