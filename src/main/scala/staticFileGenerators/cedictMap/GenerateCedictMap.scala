package staticFileGenerators.cedictMap

import ElementGenerator.ElementTranslateToAlphabet
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, StaticFileCharInfoWithLetterConway}

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

class GenerateCedictMap {
  //generateTranslatedAllChars   ElementTranslateToAlphabet
  
  def generateCedictFromFile(filePath: String, 
                             staticFileMap: Map[Grapheme, StaticFileCharInfoWithLetterConway]): 
  Set[CedictEntry] = {
    val failed: ListBuffer[CedictEntry] = ListBuffer()
    val res: ListBuffer[CedictEntry] = ListBuffer()
    
    val bufferedSource = Source.fromFile(filePath)
    val lines = bufferedSource.getLines
    for (eachline <- lines) {
      if (!eachline.startsWith("#")) {
        val words: Array[String] = eachline.split("\\s+")
        val tradEntry = new CedictEntry(words(0), CharSystem.Tzai, staticFileMap)
        val simpEntry = new CedictEntry(words(1), CharSystem.Junda, staticFileMap)
        if (!tradEntry.unambigous.isEmpty) {
          res.append(tradEntry)
        } else {
          failed.append(tradEntry)
        }
        if (!simpEntry.unambigous.isEmpty) {
          res.append(simpEntry)
        } else {
          failed.append(simpEntry)
        }
      }
    }
    bufferedSource.close()
    failed.toSet
  }
  
  def generateList(): Set[CedictEntry] = {
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
        val tradEntry = new CedictEntry(words(0), CharSystem.Tzai, trans2)
        val simpEntry = new CedictEntry(words(1), CharSystem.Junda, trans2)
        if (!tradEntry.unambigous.isEmpty) {
          res.append(tradEntry)
        } else {
          failed.append(tradEntry)
        }
        if (!simpEntry.unambigous.isEmpty) {
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
    readyToWrote.addAll(failedStr)

    val filePath = "src/main/scala/staticFileGenerators/Conway/failed.txt"
    return res.toSet
  }

  def writeSetToFile(set: List[String], filePath: String): Unit = {
    val fileWriter = new PrintWriter(new File(filePath))
    try {
      set.foreach(fileWriter.println)
    } finally {
      fileWriter.close()
    }
  }

}

object GenerateCedictMap {
  private val generate = new GenerateCedictMap()
  val cedictCompleteSet: Set[CedictEntry] = generate.generateList()
}
