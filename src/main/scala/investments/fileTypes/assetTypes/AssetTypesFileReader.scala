package sisgrana
package investments.fileTypes.assetTypes

import investments.files.{DataPath, SSV}
import java.io.{File, FileInputStream, InputStream}
import utils.use

object AssetTypesFileReader {
  val FileName = "types.ssv"

  def read(): Seq[(String, AssetType)] =
    use(new FileInputStream(new File(DataPath, FileName))) { inputStream =>
      readFrom(inputStream).toSeq
    }

  def readFrom(inputStream: InputStream): Iterator[(String, AssetType)] =
    SSV.readFrom(inputStream)
      .map { lineValues =>
        SSV.matchValues(lineValues) { case Seq(asset, typeCode) =>
          asset -> AssetType.ByCode(typeCode)
        }
      }
}
