package Adatasources.FileReaders

import Atypes.Agrapheme

import scala.collection.immutable.HashMap
import scala.collection.{immutable, mutable}
import scala.collection.mutable.HashMap
import scala.io.Source

object AtzaiData {

  val tzaiFilePath = "src/main/scala/staticFileGenerators/staticFiles/Tzai2006.txt"

  val mapJundaData = generateMapTzaiData()

  def generateMapTzaiData(): immutable.HashMap[String, Int] = {


    val bufferedSource = Source.fromFile(tzaiFilePath)
    val lines: List[String] = bufferedSource.getLines.toList

    val resultMap = new mutable.HashMap[String, Int]()

    var linenum = 0
    for (line <- lines) {
      val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      val Array(field1, field2, _*) = processedLine.split(" ")
      resultMap.put(field1, (lines.size - linenum)) //valInteger.valueOf(field3))
      linenum += 1
    }

    bufferedSource.close()
    return immutable.HashMap.from(resultMap)
  }


}
