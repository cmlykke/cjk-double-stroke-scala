package staticFileGenerators.cedictMap

import ElementGenerator.ElementTranslateToAlphabet
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, StaticFileCharInfoWithLetterConway}

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class GenerateCedictMap {
  
  def generateCedictList(): Set[CedictEntry] = {
    val idsFilePath = "src/main/scala/staticFileGenerators/staticFiles/cedict_ts.u8" // replace with your actual file path
    val radicalSupplement = "src/main/scala/staticFileGenerators/staticFiles/radicals1.txt"

    val tras = new ElementTranslateToAlphabet() //ElementTranslateToAlphabet
    val allch: Set[StaticFileCharInfoWithLetterConway] = tras.generateTranslatedAllChars() //generateTranslatedAllChars   ElementTranslateToAlphabet
    val trans2: Map[Grapheme, StaticFileCharInfoWithLetterConway] = tras.createMapFromSet(allch)

    val res: ListBuffer[CedictEntry] = ListBuffer()
    val failed: ListBuffer[CedictEntry] = ListBuffer()

    val bufferedSource = Source.fromFile(idsFilePath)
    val lines = bufferedSource.getLines
    for (eachline <- lines) {
      if (!eachline.startsWith("#")) {
        val words: Array[String] = eachline.split("\\s+")
        if (words(0) == "迈克尔·杰克逊") {
          
        }
        val meaning: String = captureBetweenStrings(eachline, "/", "/",false)
        val pronounciation: String = captureBetweenStrings(eachline, "[", "]",true)
        val tradSimp: String = words(0) + "|" + words(1) 
        val tradEntry = new CedictEntry(words(0), CharSystem.Tzai, trans2,meaning,pronounciation, tradSimp)
        val simpEntry = new CedictEntry(words(1), CharSystem.Junda, trans2,meaning,pronounciation, tradSimp)
        if (!tradEntry.unambigous.isEmpty  && tradEntry.chineseStr != "□") {
          res.append(tradEntry)
        } else {
          failed.append(tradEntry)
        }
        if (!simpEntry.unambigous.isEmpty  && simpEntry.chineseStr != "□") {
          res.append(simpEntry)
        } else {
          failed.append(simpEntry)
        }
      }
    }
    bufferedSource.close()

    val bufferedSourceRadicals = Source.fromFile(radicalSupplement)
    val linesRadicals: String = bufferedSourceRadicals.mkString("")
    val graphemesFromRad: Set[Grapheme] = Grapheme.splitIntoGraphemes(linesRadicals).map(x => Grapheme(x)).toSet
    //val filteredRads: Set[Grapheme] = graphemesFromRad.filter(x => !trans2.contains(x)).toSet
    val uniqueRad: Set[Grapheme] = graphemesFromRad.filter(x => !trans2.contains(x)).toSet
    val stringRads: Set[String] = uniqueRad.map(x => x.char).toSet

    val failedGraphs: Set[Grapheme] = failed.map(x => x.chineseStrGraphemes).flatten.toSet
    val uniqueFailed: Set[Grapheme] = failedGraphs.filter(x => !trans2.contains(x)).toSet
    val failedStr: Set[String] = uniqueFailed.map(x => x.char)

    val readyToWrote: ListBuffer[String] = new ListBuffer[String]()
    readyToWrote.addAll(stringRads)
    //readyToWrote.addAll(failedStr)

    val filePath = "src/main/scala/staticFileGenerators/Conway/failed.txt"
    return res.toSet
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
  val cedictCompleteSet: Set[CedictEntry] = generate.generateCedictList()
  val cedictMap: Map[String, CedictEntry] = generate.generateMap(cedictCompleteSet)
}
