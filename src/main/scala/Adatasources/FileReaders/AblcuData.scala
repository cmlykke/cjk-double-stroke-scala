package Adatasources.FileReaders

import scala.collection.{immutable, mutable}
import scala.io.Source

object AblcuData {


  val blcuFilePath = "src/main/scala/staticFileGenerators/staticFiles/global_wordfreq.release_UTF-8.txt"

  def generateMapBLCUData(): immutable.HashMap[String, Int] = {

    val bufferedSource = Source.fromFile(blcuFilePath)
    val lines: List[String] = bufferedSource.getLines.toList

    val resultMap = new mutable.HashMap[String, Int]()

    var countEach = 1
    for (line <- lines) {
      val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      val Array(field1, field2, _*) = processedLine.split("\t")
      resultMap.put(field1, countEach) //valInteger.valueOf(field3))
      countEach = countEach + 1 
    }

    bufferedSource.close()
    return immutable.HashMap.from(resultMap)
  }
}
