package GenerateOutput

import OutputTranslation.OutputSorting
import UtilityClasses.OutputEntry
import staticFileGenerators.SpecialCharacters.ReadSpecialCharacters

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class GenerateOutputStrings {

  private def removeDuplicateLines(input: ListBuffer[String]): List[String] = {
    val seen: mutable.Set[String] = mutable.Set()
    var result: ListBuffer[String] = ListBuffer[String]()
    val dublicates: mutable.Set[String] = mutable.Set()
    val chineseZero = input.filter(x => x.startsWith("z\t"))

    for (line <- input) {
      if (!seen.contains(line)) {
        result.append(line)
        seen.add(line)
      } else {
        dublicates.add(line)
      }
    }
    result.toList
  }

  private def generateSpecialCharacterStrList(): ListBuffer[String] = {
    val out = ListBuffer[String]()
    // Add special characters
    for (eachSpechar <- GenerateOutputStrings.specialChars) {
      val allCodes: List[String] = eachSpechar.codes.toList.sorted
      for (eachCode <- allCodes) {
        val outStr: String = eachCode + "\t" + eachSpechar.chineseStr
        out += outStr
      }
    }
    out
  }

  def writeListToFile(lines: List[String], fileName: String, relativePath: String): Try[Unit] = {
    val directory = new File(relativePath)
    if (!directory.exists()) {
      directory.mkdirs()
    }

    val file = new File(directory, fileName)
    val bw = new BufferedWriter(new FileWriter(file))

    Try {
      for (line <- lines) {
        bw.write(line)
        bw.newLine()
      }
    } match {
      case success @ Success(_) =>
        bw.close()
        success
      case failure @ Failure(_) =>
        bw.close()
        failure
    }
  }

  def generateWithSpecial(input: SortedMap[String, List[OutputEntry]]): List[String] = {
    val out: ListBuffer[String] = generateSpecialCharacterStrList()
    for ((eachKey, valList) <- input) {
      for (singleVal <- valList) {
        val eachLine: String = eachKey + "\t" + singleVal.chineseStr
        out += eachLine
      }
    }
    val finalOut: List[String] = removeDuplicateLines(out)
    finalOut
  }

  def generateWithMeaning(input: SortedMap[String, List[OutputEntry]]): List[String] = {
    val out: ListBuffer[String] = ListBuffer[String]()
    for ((eachKey, valList) <- input) {
      for (singleVal <- valList) {
        val showStr: String = if (singleVal.tradSimp.isBlank) singleVal.chineseStr else singleVal.tradSimp
        val outputLine: String =
          showStr+"_"+
          singleVal.pron+"_"+
          singleVal.meaning
        val eachLine: String = eachKey + "\t" + outputLine.trim.replace(' ', '_')
        out += eachLine
      }
    }
    val finalOut: List[String] = removeDuplicateLines(out)
    finalOut
  }



}

object GenerateOutputStrings {
  val specialChars: List[OutputEntry] = OutputSorting.specialChars
  val mapFullJunda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
  val mapFullTzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai
}
