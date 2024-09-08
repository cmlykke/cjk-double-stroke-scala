package staticFileGenerators.TzaiFrequency

import scala.collection.mutable.HashMap
import scala.io.Source

case class TzaiData(ordinal: Int, frequency: Double)

class GenerateTzaiMap {
  
  import GenerateTzaiMap._
  def getTzaiMap(): HashMap[String, TzaiData] = {
    return mapTzaiData
  }
  
  def getSumThirdColumn(): Long = {
    return sumSecondColumn()
  }
  
}

object GenerateTzaiMap {
  
  val tzaiFilePath = "src/main/scala/staticFileGenerators/staticFiles/Tzai2006.txt"
  
  val mapTzaiData: HashMap[String, TzaiData] = generateMapTzaiData()


  def sumSecondColumn(): Long = {
    val source = Source.fromFile(tzaiFilePath)
    val sum = source.getLines().flatMap { line =>
      val splitLine = line.split("\\s+")
      if (splitLine.length > 1) Some(splitLine(1).toLong)
      else None
    }.sum
    source.close()
    return sum
  }

  def generateMapTzaiData(): HashMap[String, TzaiData] = {
    val allchars = sumSecondColumn()
    val bufferedSource = Source.fromFile(tzaiFilePath)
    val lines = bufferedSource.getLines

    val resultMap = new HashMap[String, TzaiData]()
    var index = 0

    for (line <- lines) {
      val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      val Array(field1, field2, _*) = processedLine.split("\\s+")

      resultMap.put(field1, TzaiData((index + 1).toInt, field2.toDouble / allchars))
      index += 1
    }

    bufferedSource.close()
    return resultMap
  }
  
  
  
}