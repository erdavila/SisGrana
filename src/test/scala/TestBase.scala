package sisgrana

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

trait TestBase extends AnyFunSuite with TableDrivenPropertyChecks with Matchers
