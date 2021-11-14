package sisgrana

import investments.model.StockbrokerAsset
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import utils.CustomMatchers

trait TestBase extends AnyFunSuite with TableDrivenPropertyChecks with Matchers with CustomMatchers

object TestBase {
  val DefaultAsset = "default-asset"
  val DefaultStockbroker = "default-stockbroker"
  val DefaultStockbrokerAsset: StockbrokerAsset = StockbrokerAsset(DefaultStockbroker, DefaultAsset)
}
