package staticFileGenerators.BLCUmap

import staticFileGenerators.Academiasinica.SinicaData

import UtilityClasses.Grapheme
import staticFileGenerators.TzaiFrequency.TzaiData
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{HashMap, LinkedHashMap, ListBuffer}
import scala.io.Source

case class BLCUData(ordinal: Int, frequency: Double)

class GenerateBLCUmap {

}

object GenerateBLCUmap {
  val blcupath = "src/main/scala/staticFileGenerators/staticFiles/global_wordfreq.release_UTF-8.txt"

  val blcuMap: LinkedHashMap[String, BLCUData] = generateSinicaMap(blcupath)
  
  
  private def generateSinicaMap(path: String): LinkedHashMap[String, BLCUData] = {
    val input: List[(String, Int, Double)] = getChineseLines(path)
    var res = LinkedHashMap[String, BLCUData]()
    var doubleres: List[(String, BLCUData)] = List()
    for (eachTupple <- input) {
      if (res.contains(eachTupple._1)) {
        //doubleres.appended((word, res.get(word).get))
        //doubleres.appended((word, BLCUData(index)))
      } else {
        res.addOne((eachTupple._1, BLCUData(eachTupple._2, eachTupple._3)))
      }
    }
    res
  }
  
  def getChineseLines(path: String): List[(String, Int, Double)] = {
    var res = ListBuffer[(String, Int, Double)]()
    val source = Source.fromFile(path)
    val chineseLines = source.getLines().toList
    var count: Int = 1
    for (eachLine <- chineseLines) {
      val splitLine = eachLine.split('\t')
      if (splitLine.length > 1) {
        val freq: Double = splitLine(1).toDouble
        res.addOne((splitLine(0), count, freq))
        count += 1
      }
    }
    return res.toList
  }

}
