package sisgrana
package investments.irpf

import java.io.File

sealed abstract class Type(val code: String) {
  def variableRate: Boolean
  def exemptable: Boolean
  def fii: Boolean
}

object Type {
  case object Stock extends Type("") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = true
    override def fii: Boolean = false
  }

  case object ETF extends Type("ETF") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = false
    override def fii: Boolean = false
  }

  case object EtfRendaFixa extends Type("ETFRF") {
    override def variableRate: Boolean = false
    override def exemptable: Boolean = false
    override def fii: Boolean = false
  }

  case object FII extends Type("FII") {
    override def variableRate: Boolean = true
    override def exemptable: Boolean = false
    override def fii: Boolean = true
  }

  val ByCode: Map[String, Type] = Set(Stock, ETF, EtfRendaFixa, FII)
    .map(tp => tp.code -> tp)
    .toMap

  val Default: Type = Stock
}

class Types(typesByAssetCode: Map[String, Type]) {
  def apply(assetCode: String): Type =
    typesByAssetCode.getOrElse(assetCode, Type.Default)
}

object Types {
  def fromFile(file: File): Types = {
    val typesByAssetCode = TSV.fromFile(file) { lines =>
      (
        for {
          List(assetCode, typeCode) <- lines
          tp = Type.ByCode(typeCode)
        } yield assetCode -> tp
      ).toMap
    }
    new Types(typesByAssetCode)
  }
}
