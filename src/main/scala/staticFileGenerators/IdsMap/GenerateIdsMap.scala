package staticFileGenerators.IdsMap

import UtilityClasses.{Cluster, Grapheme}
import staticFileGenerators.JundaFrequency.JundaData

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.io.{BufferedSource, Source}

class GenerateIdsMap {
  val idsFilePath = "src/main/scala/staticFileGenerators/staticFiles/ids.txt" // replace with your actual file path
  val idsManualFilePath = "src/main/scala/staticFileGenerators/staticFiles/manualidsIDS.txt"
  val idsFilePathMissing = "src/main/scala/staticFileGenerators/IdsMap/orderedMissingIds.txt"
  
  def mapIdsData(): HashMap[Grapheme, List[Cluster]] = {
    val linesMissing: ListBuffer[String] = getIdsList(idsFilePathMissing)
    val lines: ListBuffer[String] = getIdsList(idsFilePath) //U+9C83	鲃	⿰鱼巴
    val linesManual: ListBuffer[String] = getIdsList(idsManualFilePath)

//    445343511554(1|4)11125155431545513154224544(454|4454|4554)
    // U+4E21	両	⿱一⿻冂山
    //lines.append("U+2A88C\t𪢌\t⿰口阑")   //  U+2A88C	𪢌	⿰口阑
    // ListBuffer[String] = lines.to(ListBuffer)
    
    //add lines 阑
    //resultMap.put(Grapheme("𪢌"), ConwayColl(List("251(245|425)125431234"), "U+2A88C", Grapheme("𪢌")))
    // 𪢌 U+2A88C  // 阑 问  "𪢌"
    val resultMap_idsMissing = getMapFromList(linesMissing)
    val resultMap_ids = getMapFromList(lines)
    val resultMap_idsManual = getMapFromList(linesManual)

    resultMap_idsMissing.addAll(resultMap_ids)
    resultMap_idsMissing.addAll(resultMap_idsManual)

    return resultMap_idsMissing
  }

  private def getMapFromList(lines: ListBuffer[String]): mutable.HashMap[Grapheme, List[Cluster]] = {
    val resultMap = new mutable.HashMap[Grapheme, List[Cluster]]()

    for (line <- lines) {
      val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      val splitLine = processedLine.split("\\s")
      val field2 = splitLine(1)
      if (Grapheme.isGrapheme(field2)) {
        val restOfTheFields = splitLine.drop(2).map(_.replaceAll("[\\p{ASCII}]", "")).toList
        resultMap.put(Grapheme(field2), restOfTheFields.map(x => Cluster(x)).toList)
      } else {
        print(field2)
      }

    }
    return resultMap
  }


  private def getIdsList(idsFilePath: String): ListBuffer[String] = {
    val bufferedSource = Source.fromFile(idsFilePath)
    val linesOriginal: ListBuffer[String] = bufferedSource.getLines.to(ListBuffer)
    bufferedSource.close()
    return linesOriginal.filter(x => !x.isEmpty)
  }
}
