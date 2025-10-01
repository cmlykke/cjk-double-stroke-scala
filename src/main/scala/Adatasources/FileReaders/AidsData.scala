package Adatasources.FileReaders

import Atypes.{AcedictColl, AconwayColl, Agrapheme, AidsRecur}

import scala.collection.immutable.HashMap
import scala.collection.{immutable, mutable}
import scala.io.Source

object AidsData {

  def idsData(): HashMap[Agrapheme, AidsRecur] = {
    val raw: HashMap[Agrapheme, String] = idsDataRaw()
    val result: HashMap[Agrapheme, AidsRecur] = generateRecurMap(raw)
    result
  }

  def generateRecurMap(input: HashMap[Agrapheme, String]):  HashMap[Agrapheme, AidsRecur] = {
    var resultMap = new mutable.HashMap[Agrapheme, AidsRecur]()

    for ((mykey, _) <- input) {
      resultMap.put(mykey, AidsRecur(mykey, input))
    }
    immutable.HashMap.from(resultMap)
  }

  def idsDataRaw(): HashMap[Agrapheme, String] = {
    val idsFilePath = "src/main/scala/staticFileGenerators/staticFiles/ids.txt"
    val manualIdsFilePath = "src/main/scala/staticFileGenerators/staticFiles/manualidsIDS.txt"

    val bufferedSource = Source.fromFile(idsFilePath)
    val officiallines: List[String] = bufferedSource.getLines.toList
    bufferedSource.close()

    val bufferedSourcemanualIdsFilePath = Source.fromFile(manualIdsFilePath)
    val manuallines: List[String] = bufferedSourcemanualIdsFilePath.getLines.toList
    bufferedSourcemanualIdsFilePath.close()

    val data: HashMap[Agrapheme, String] = AidsData.createIdsMap(officiallines ++ manuallines)
    data
  }

  def createIdsMap(lines: List[String]): immutable.HashMap[Agrapheme, String] = {
    var resultMap = new mutable.HashMap[Agrapheme, String]()

    for (line <- lines) {
      if (line.startsWith("U+")) {
        val splitLine = line.split("\\s")
        val unicodehex = splitLine(0)
        val grapheme = splitLine(1)
        val firstrecur = splitLine(2)
        resultMap.put(Agrapheme(grapheme), firstrecur)
      }
    }
    immutable.HashMap.from(resultMap)
  }
}
