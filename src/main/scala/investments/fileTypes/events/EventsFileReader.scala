package sisgrana
package investments.fileTypes.events

import investments.fileTypes.events.Event.{Bonus, Conversion}
import investments.files.SSV
import java.io.InputStream
import utils.BrNumber

object EventsFileReader {
  def readFrom(inputStream: InputStream): Iterator[Event] =
    for (lineValues <- SSV.readFrom(inputStream))
      yield parseLineValues(lineValues)

  private def parseLineValues(lineValues: SSV.LineValues): Event =
    SSV.matchValues(lineValues) {
      case Seq("convert", fromAsset, fromQty, "->", toQty, toAsset) =>
        Conversion(fromAsset, BrNumber.parse(fromQty), toAsset, BrNumber.parse(toQty))
      case Seq("bonus", fromAsset, fromQty, "->", toQty, toAsset, toPrice) =>
        Bonus(fromAsset, BrNumber.parse(fromQty), toAsset, BrNumber.parse(toQty), BrNumber.parse(toPrice))
    }
}
