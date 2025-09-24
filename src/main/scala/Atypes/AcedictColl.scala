package Atypes

class AcedictColl(cedictLines: List[String]) {
  val simplified: Set[AcedictEntry] = AcedictColl.createSimplifiedEntreis(cedictLines)
  val traditional: Set[AcedictEntry] = AcedictColl.createTraditioalEntreis(cedictLines)
}

object AcedictColl {

  def createTraditioalEntreis(cedictLines: List[String]): Set[AcedictEntry] = {
    val validated = getValidatedLinesFromText(cedictLines)
    validated
      .map(lines => lines.split("\\s+"))
      .map(_(1))
      .map(getEntryFromString(_)).toSet
  }
  
  def createSimplifiedEntreis(cedictLines: List[String]): Set[AcedictEntry] = {
    val validated = getValidatedLinesFromText(cedictLines)
    validated
      .map(lines => lines.split("\\s+"))
      .map(_(1))
      .map(getEntryFromString(_)).toSet
  }
  
  def getValidatedLinesFromText(rawCdictLines: List[String]): List[String] = {
    rawCdictLines.filter(line => !line.startsWith("#"))
  }
  
  def getEntryFromString(input: String): AcedictEntry = {
    AcedictEntry(input)
  }
}

