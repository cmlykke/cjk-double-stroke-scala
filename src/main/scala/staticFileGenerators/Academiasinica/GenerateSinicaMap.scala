package staticFileGenerators.Academiasinica

import UtilityClasses.Grapheme
import staticFileGenerators.TzaiFrequency.TzaiData

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{HashMap, LinkedHashMap, ListBuffer}
import scala.io.Source

case class SinicaData(ordinal: Int)

class GenerateSinicaMap {

}


object GenerateSinicaMap {

  val as1000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as1000.txt"
  val as2000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as2000.txt"
  val as3000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as3000.txt"
  val as4000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as4000.txt"
  val as5000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as5000.txt"
  val as6000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as6000.txt"
  val as7000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as7000.txt"
  val as8000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as8000.txt"
  val as9000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as9000.txt"
  val as10000 = "src/main/scala/staticFileGenerators/staticFiles/academiasinica/as10000.txt"

  val sinicaList: List[String] = retrieveFirstColumn()
  val sinicaGraphemeSet: Set[String] = generateSinicaSet(sinicaList)
  val sinicaMap: LinkedHashMap[String, SinicaData] = generateSinicaMap(sinicaList)
  val sinicaSet: Set[String] = generateSinicaSet(sinicaList)

  private def generateSinicaMap(input: List[String]): LinkedHashMap[String, SinicaData] = {
    var res = LinkedHashMap[String, SinicaData]()
    var doubleres: List[(String, SinicaData)] = List()
    for (word, index) <- input.zipWithIndex do
      if (res.contains(word)) {
        doubleres.appended((word, res.get(word).get))
        doubleres.appended((word, SinicaData(index)))
      } else {
        res.addOne((word, SinicaData(index)))
      }
    res
  }

  private def generateSinicaSet(input: List[String]): Set[String] = {
    var res = ListBuffer[String]()
    for (eachWord <- input) {
      var splitlist = splitIntoGraphemes(eachWord)
      for (eachChar <- splitlist) {
        res.addOne(eachChar)
      }
    }
    return res.toSet
  }

  private def splitIntoGraphemes(input: String): List[String] = {
    val graphemeRegex = "\\X".r
    val res = graphemeRegex.findAllIn(input).toList
    if (input == "专") {
      val tes = ""
    }
    return res
  }

  private def retrieveFirstColumn(): List[String] = {
    val line1 = getChineseLines(as1000)
    val line2 = getChineseLines(as2000)
    val line3 = getChineseLines(as3000)
    val line4 = getChineseLines(as4000)
    val line5 = getChineseLines(as5000)
    val line6 = getChineseLines(as6000)
    val line7 = getChineseLines(as7000)
    val line8 = getChineseLines(as8000)
    val line9 = getChineseLines(as9000)
    val line10 = getChineseLines(as10000)
    var res = ListBuffer[String]()
    res.addAll(line1)
    res.addAll(line2)
    res.addAll(line3)
    res.addAll(line4)
    res.addAll(line5)
    res.addAll(line6)
    res.addAll(line7)
    res.addAll(line8)
    res.addAll(line9)
    res.addAll(line10)
    return res.toList

  }

  def getChineseLines(path: String): List[String] = {
    var res = ListBuffer[String]()
    val source = Source.fromFile(path)
    val chineseLines = source.getLines().toList
    for (eachLine <- chineseLines) {
      val splitLine = eachLine.split('|')
      if (splitLine.length > 1
        && splitLine(0).length == 0
        && splitLine(1).length > 0) {
        res.addOne(splitLine(1))
      }
    }
    return res.toList
  }

}
