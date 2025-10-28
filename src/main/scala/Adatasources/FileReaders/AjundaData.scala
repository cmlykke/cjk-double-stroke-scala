package Adatasources.FileReaders

import Atypes.Agrapheme

import scala.collection.immutable.HashMap
import scala.collection.{immutable, mutable}
import scala.collection.mutable.HashMap
import scala.io.Source

object AjundaData {
  val jundaFilePath = "src/main/scala/staticFileGenerators/staticFiles/Junda2005.txt" // replace with your actual file path

  val mapJundaData = generateMapJundaData()


  def generateMapJundaData(): immutable.HashMap[String, Int] = {

    val bufferedSource = Source.fromFile(jundaFilePath)
    val lines: List[String] = bufferedSource.getLines.toList

    val resultMap = new mutable.HashMap[String, Int]()

    var linenum = 0
    for (line <- lines) {
      val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      val Array(field1, field2, field3, field4, _*) = processedLine.split("\t")
      resultMap.put(field2, (lines.size - linenum))//valInteger.valueOf(field3))
      linenum += 1
    }

    bufferedSource.close()
    return immutable.HashMap.from(resultMap)
  }

}
