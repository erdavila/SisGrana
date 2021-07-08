package sisgrana
package investments.variableIncome

import investments.variableIncome.importAssets.SSV
import java.io.File

sealed abstract class AssetType(val code: String) {
  def variableRate: Boolean
  def exemptable: Boolean
  def fii: Boolean
}

object AssetType {
  case object Stock extends AssetType("") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = true
    override def fii: Boolean = false
  }

  case object ETF extends AssetType("ETF") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = false
    override def fii: Boolean = false
  }

  case object EtfRendaFixa extends AssetType("ETFRF") {
    override def variableRate: Boolean = false
    override def exemptable: Boolean = false
    override def fii: Boolean = false
  }

  case object FII extends AssetType("FII") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = false
    override def fii: Boolean = true
  }

  case object Option extends AssetType("option") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = false
    override def fii: Boolean = false
  }

  val ByCode: Map[String, AssetType] = Set(Stock, ETF, EtfRendaFixa, FII)
    .map(tp => tp.code -> tp)
    .toMap

  val Default: AssetType = Stock

  class Resolver(typesByAssetCode: Map[String, AssetType]) {
    def apply(assetCode: String): AssetType = resolve(assetCode)

    def resolve(assetCode: String): AssetType =
      if (Resolver.isOption(assetCode)) Option
      else typesByAssetCode.getOrElse(assetCode, AssetType.Default)
  }

  object Resolver {
    private val OptionRegex = """^[A-Z0-9]{4}[A-X]\d{3}$""".r

    def fromFile(file: File): Resolver = {
      val entries = SSV.readFile(file).map { lineValues =>
        SSV.matchValues(lineValues) { case Seq(asset, typeCode) =>
          asset -> AssetType.ByCode(typeCode)
        }
      }
      new Resolver(entries.toMap)
    }

    def isOption(assetCode: String): Boolean =
      OptionRegex.matches(assetCode)
  }
}
