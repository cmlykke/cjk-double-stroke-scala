package Adatasources.FileReaders

import Atypes.{AcedictColl, Aconway, AconwayColl, Agrapheme}
import UtilityClasses.{ConwayColl, Grapheme}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable.HashMap
import scala.io.Source

object AreadCedictData {

  def listCedictData(): AcedictColl = {
    val cedictFilePath = "src/main/scala/staticFileGenerators/staticFiles/cedict_ts.u8" // replace with your actual file path
    val radicalSupplement = "src/main/scala/staticFileGenerators/staticFiles/radicals1.txt"

    val bufferedSource = Source.fromFile(cedictFilePath)
    val lines: List[String] = bufferedSource.getLines.toList
    bufferedSource.close()

    val data: Set[String] = AreadConwayData.conwaySetFunc()
    
    AcedictColl(lines, data)
  }
}
