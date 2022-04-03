package sisgrana
package investments

import investments.fileTypes.assetTypes.AssetTypesFileReader
import utils.quoted

sealed abstract class AssetType(val code: String) {
  def taxation: Taxation
}

object AssetType {
  case object Stock extends AssetType("") {
    override def taxation: Taxation = Taxation.ExemptableSwingTrade
  }

  case object ETF extends AssetType("ETF") {
    override def taxation: Taxation = Taxation.NonExemptableStocksSwingTrade
  }

  case object EtfRendaFixa extends AssetType("ETFRF") {
    override def taxation: Taxation = Taxation.NonVariableIncome
  }

  case object FII extends AssetType("FII") {
    override def taxation: Taxation = Taxation.FII
  }

  case object Option extends AssetType("option") {
    override def taxation: Taxation = Taxation.NonExemptableOptionsSwingTrade

    sealed trait Type

    object Type {
      case object Call extends Type
      case object Put extends Type
    }

    def typeOf(option: String): Type = {
      val typeChar = option(4)
      if (('A' to 'L') `contains` typeChar) {
        Type.Call
      } else if (('M' to 'X') `contains` typeChar) {
        Type.Put
      } else {
        throw new Exception(s"Código inválido para opção ${quoted(option)}: $typeChar")
      }
    }
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
    private val OptionRegex = """^[A-Z0-9]{4}[A-X]\d{2,3}$""".r

    def get(): Resolver = {
      val entries = AssetTypesFileReader.read()
      new Resolver(entries.toMap)
    }

    def isOption(assetCode: String): Boolean =
      OptionRegex.matches(assetCode)
  }
}
