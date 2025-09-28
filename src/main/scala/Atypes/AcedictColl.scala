package Atypes

import scala.collection.mutable.HashMap

class AcedictColl(cedictLines: List[String], conwaySet: Set[String]) {
  val allSingleCedictCharacters: Set[AcedictEntry] = AcedictColl.createSingles(cedictLines)
  val simplifiedWords: Set[AcedictEntry] = AcedictColl.createSimplifiedEntreis(cedictLines, conwaySet)
  val traditionalWords: Set[AcedictEntry] = AcedictColl.createTraditioalEntreis(cedictLines, conwaySet)
  val simplifiedAllHanItems: Set[AcedictEntry] = AcedictColl.splitIntoSingleHanEntries(simplifiedWords)
  val traditionalAllHanItems: Set[AcedictEntry] = AcedictColl.splitIntoSingleHanEntries(traditionalWords)
  val allHanCharacters: Set[AcedictEntry] = simplifiedAllHanItems ++ traditionalAllHanItems
}

object AcedictColl {

  def createSingles(cedictLines: List[String]): Set[AcedictEntry] = {
    val validated = getValidatedLinesFromText(cedictLines)
    validated
      .map(lines => lines.split("\\s+"))
      .flatMap(arr => Seq(arr.lift(0), arr.lift(1)).flatten) // Get first two elements safely
      .filter(_.codePoints().toArray.length == 1)
      .map(getEntryFromString)
      .toSet
  }
  def createTraditioalEntreis(cedictLines: List[String],
                              conwaySet: Set[String]): Set[AcedictEntry] = {
    val validated = getValidatedLinesFromText(cedictLines)
    validated
      .map(lines => lines.split("\\s+"))
      .map(_(0))
      .filter(str => (str.codePoints().toArray.length > 1) || (conwaySet.contains(str)))
      .map(getEntryFromString(_)).toSet
  }
  
  def createSimplifiedEntreis(cedictLines: List[String],
                              conwaySet: Set[String]): Set[AcedictEntry] = {
    val validated = getValidatedLinesFromText(cedictLines)
    validated
      .map(lines => lines.split("\\s+"))
      .map(_(1))
      .filter(str => (str.codePoints().toArray.length > 1) || (conwaySet.contains(str)))
      .map(getEntryFromString(_)).toSet
  }

  def splitIntoSingleHanEntries(input: Set[AcedictEntry]): Set[AcedictEntry] = {
    input.map(entry => entry.rawEntry.codePoints().toArray).flatten.toSet
      .filter(number => number > 127)
      .map(removedAscii => Character.toString(removedAscii))
      .map(singleCharacterText => AcedictEntry(singleCharacterText))
  }

  def getValidatedLinesFromText(rawCdictLines: List[String]): List[String] = {
    rawCdictLines.filter(line => !line.startsWith("#"))
  }
  
  def getEntryFromString(input: String): AcedictEntry = {
    AcedictEntry(input)
  }
}

