package UtilityClasses

import com.ibm.icu.text.BreakIterator
import com.ibm.icu.util.ULocale
import staticFileGenerators.JundaFrequency.{GenerateJundaMap, JundaData}
import staticFileGenerators.TzaiFrequency.{GenerateTzaiMap, TzaiData}

import scala.io.Source

class Grapheme(input: String) {
  val char: String = Grapheme.verifyGrapheme(input)
  val isShape: Boolean = Grapheme.isShapeCharacter(input)
  val junda: Option[JundaData] = Grapheme.jundaMap.get(char)
  val tzai: Option[TzaiData] = Grapheme.tzaiMap.get(char)
  val unicode: List[String] = stringToHexUnicodeList(input)

  private def stringToHexUnicodeList(input: String): List[String] = {
    input.codePoints()
      .toArray
      .map(codePoint => "U+" + codePoint.toHexString.toUpperCase)
      .toList
  }

  override def equals(obj: Any): Boolean = obj match {
    case g: Grapheme => g.char == this.char
    case _           => false
  }

  override def hashCode(): Int = char.hashCode
}

object Grapheme {

  val jundaMap = GenerateJundaMap().getJundaMap()
  val tzaiMap = GenerateTzaiMap().getTzaiMap()

  def filepathToGraphemeSet(filepath: String): Set[Grapheme] = {
    val fileStr: String = Source.fromFile(filepath).mkString("")
    Grapheme.splitIntoGraphemes(fileStr).map(x => Grapheme(x)).toSet
  }
  
  def splitIntoGraphemes(input: String): List[String] = {
    val graphemeRegex = "\\X".r
    val res = graphemeRegex.findAllIn(input).toList
    if (input == "专") {
      val tes = ""
    }
    return res
  }

  def isGrapheme(input: String): Boolean = {
    val res = splitIntoGraphemes(input)
    return res.length == 1
  }

  def verifyGrapheme(input: String): String = {
    if (!isGrapheme(input)) {
      throw new IllegalArgumentException("Input must be exactly one grapheme cluster: " + input)
    }
    input
  }

  def isShapeCharacter(input: String): Boolean = {
    if (input.length != 1) {
      false
    } else {
      val codePoint = input.codePointAt(0)
      codePoint >= 0x2FF0 && codePoint <= 0x2FFF
    }
  }
}

