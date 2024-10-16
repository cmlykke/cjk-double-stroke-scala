package staticFileGenerators.HeisigMap

import UtilityClasses.Grapheme

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class GenerateHeisigMap {

}

object GenerateHeisigMap {
  val heisigFilePath = "src/main/scala/staticFileGenerators/staticFiles/heisigTXT.txt"

  val nestedList: List[List[String]] = getChineseLines(heisigFilePath)
  val heisigSimplified: Map[Grapheme, Int] = createMap(1, 4, nestedList)
  val heisigTraditional: Map[Grapheme, Int] = createMap(0, 3, nestedList)

  def getChineseLines(path: String): List[List[String]] = {
    val bufferedSource = Source.fromFile(path)
    val lines = bufferedSource.getLines

    val splittetLines: List[List[String]] = lines.map(x => x.split("\\t").toList).toList
    //val allGraphemes: Set[Grapheme] = lines.map(x => Grapheme.splitIntoGraphemes(x).map(y => Grapheme(y))).flatten.toSet
    return splittetLines
  }

  def createMap(numIndex: Int, charIndex: Int, nestedList: List[List[String]]): Map[Grapheme, Int] = {
    var tempres: mutable.Map[Grapheme, Int] = mutable.Map[Grapheme, Int]()
    for (each <- nestedList) {
      if (each.length > numIndex && each.length > charIndex) {
        val strChar: String = each(charIndex)
        val strNum: String = each(numIndex)
        if (canBeParsedAsInt(strNum) && strChar.length == 1) {
          tempres.put(Grapheme(strChar), numIndex.toInt)
        }
      }
    }
    return tempres.toMap
  }


  def canBeParsedAsInt(str: String): Boolean = {
    try {
      str.toInt
      true
    } catch {
      case _: NumberFormatException => false
    }
  }
}
