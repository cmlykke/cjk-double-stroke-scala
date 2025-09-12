package staticFileGenerators.cedictMap

import ElementGenerator.ElementTranslateToAlphabet
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, StaticFileCharInfoWithLetterConway}

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class GenerateCedictMap {
  
  def generateCedictList(): (Set[CedictEntry], Set[CedictEntry], Set[CedictEntry]) = {
    val idsFilePath = "src/main/scala/staticFileGenerators/staticFiles/cedict_ts.u8" // replace with your actual file path
    val radicalSupplement = "src/main/scala/staticFileGenerators/staticFiles/radicals1.txt"

    val allch: Set[StaticFileCharInfoWithLetterConway] = ElementTranslateToAlphabet.generateTranslatedAllChars() //generateTranslatedAllChars   ElementTranslateToAlphabet
    val trans2: Map[Grapheme, StaticFileCharInfoWithLetterConway] = ElementTranslateToAlphabet.createMapFromSet(allch)

    val simpBuffer: ListBuffer[CedictEntry] = ListBuffer()
    val tradBuffer: ListBuffer[CedictEntry] = ListBuffer()
    val res: ListBuffer[CedictEntry] = ListBuffer()
    val failed: ListBuffer[CedictEntry] = ListBuffer()

    val bufferedSource = Source.fromFile(idsFilePath)
    val lines = bufferedSource.getLines
    val unevenElems: ListBuffer[CedictEntry] = ListBuffer()
    var tradAdded = false
    for (eachline <- lines) {
      if (!eachline.startsWith("#")) {
        val words: Array[String] = eachline.split("\\s+")
        val meaning: String = captureBetweenStrings(eachline, "/", "/",false)
        val pronounciation: String = captureBetweenStrings(eachline, "[", "]",true)
        val tradSimp: String = words(0) + "|" + words(1) 
        val tradEntry = new CedictEntry(words(0), CharSystem.Tzai, trans2,meaning,pronounciation, tradSimp)
        val simpEntry = new CedictEntry(words(1), CharSystem.Junda, trans2,meaning,pronounciation, tradSimp)
        if (!tradEntry.unambigous.isEmpty  && tradEntry.chineseStr != "□") {
          tradBuffer.append(tradEntry)
          res.append(tradEntry)
          tradAdded = true
        } else {
          failed.append(tradEntry)
        }
        if (!simpEntry.unambigous.isEmpty  && simpEntry.chineseStr != "□") {
          simpBuffer.append(simpEntry)
          res.append(simpEntry)
          if (!tradAdded) {
            unevenElems.append(tradEntry)
          }
        } else {
          failed.append(simpEntry)
          if (tradAdded) {
            unevenElems.append(simpEntry)
          }
        }
      }
    }
    bufferedSource.close()

    return (res.toSet, tradBuffer.toSet, simpBuffer.toSet)
  }

  private def writeSetToFile(set: List[String], filePath: String): Unit = {
    val fileWriter = new PrintWriter(new File(filePath))
    try {
      set.foreach(fileWriter.println)
    } finally {
      fileWriter.close()
    }
  }

  private def captureBetweenStrings(input: String,
                            string1: String,
                            string2: String,
                            firstMatch: Boolean): String = {
    // Find the occurrence of string1
    val startIdx = input.indexOf(string1)
    // Validate startIdx
    if (startIdx == -1) {
      throw new IllegalArgumentException("Input string must contain the start delimiter.")
    }
    // Find the occurrence of string2 based on firstMatch
    val endIdx = if (firstMatch) {
      input.indexOf(string2, startIdx + string1.length)
    } else {
      input.lastIndexOf(string2)
    }
    // Validate endIdx
    if (endIdx == -1 || startIdx >= endIdx) {
      throw new IllegalArgumentException("Input string must contain the specified delimiters in the proper order.")
    }
    // Capture the substring between string1 and string2
    input.substring(startIdx + string1.length, endIdx)
  }
  
  def generateMap(cedictCompleteSet: Set[CedictEntry]): Map[String, CedictEntry] = {
    val map = mutable.Map[String, CedictEntry]()
    cedictCompleteSet.foreach { entry =>
      map += (entry.chineseStr -> entry)
    }
    map.toMap
  }

}

object GenerateCedictMap {
  private val generate = new GenerateCedictMap()
  val cedictTupple = generate.generateCedictList()
  val cedictSimpSet = cedictTupple._3
  val cedictTradSet = cedictTupple._2
  val cedictCompleteSet: Set[CedictEntry] = cedictTupple._2 ++ cedictTupple._3
  val cedictMap: Map[String, CedictEntry] = generate.generateMap(cedictCompleteSet)
}
