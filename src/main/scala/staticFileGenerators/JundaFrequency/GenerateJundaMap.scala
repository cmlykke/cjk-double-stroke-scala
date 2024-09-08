package staticFileGenerators.JundaFrequency

import UtilityClasses.StaticFileCharInfo

import scala.collection.mutable.HashMap
import scala.io.Source
import staticFileGenerators.JundaFrequency.JundaData

case class JundaData(ordinal: Int, frequency: Double)


class GenerateJundaMap {

  import GenerateJundaMap._
  def getJundaMap(): HashMap[String, JundaData] = {
    return mapJundaData
  }
  
  def getSumThirdColumn(): Long = {
    return sumThirdColumn()
  }
  
}

object GenerateJundaMap {
  
  val jundaFilePath = "src/main/scala/staticFileGenerators/staticFiles/Junda2005.txt" // replace with your actual file path

  val mapJundaData = generateMapJundaData()
  


  def generateMapJundaData(): HashMap[String, JundaData] = {

    val allchars = sumThirdColumn()

    val bufferedSource = Source.fromFile(jundaFilePath)
    val lines = bufferedSource.getLines

    val resultMap = new HashMap[String, JundaData]()

    for (line <- lines) {
      val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      val Array(field1, field2, field3, _*) = processedLine.split("\t")
      resultMap.put(field2, JundaData(field1.toInt, field3.toDouble / allchars))
    }

    bufferedSource.close()
    return resultMap
  }

  // Your other code...
  def sumThirdColumn(): Long = {
    val source = Source.fromFile(jundaFilePath)
    val sum = source.getLines().map(_.split("\t")(2).toLong).sum
    source.close()
    return sum
  }


}

