package Atypes

import com.ibm.icu.text.BreakIterator
import com.ibm.icu.util.ULocale
import staticFileGenerators.JundaFrequency.{GenerateJundaMap, JundaData}
import staticFileGenerators.TzaiFrequency.{GenerateTzaiMap, TzaiData}

import scala.io.Source

class Agrapheme(input: String) {
  val char: String = Agrapheme.verifyGrapheme(input)

  override def equals(obj: Any): Boolean = obj match {
    case g: Agrapheme => g.char == this.char
    case _           => false
  }

  override def hashCode(): Int = char.hashCode
}

object Agrapheme {

  def verifyGrapheme(input: String): String = {
    if (input.codePoints().count() != 1) {
      throw new IllegalArgumentException("Input must be exactly one grapheme cluster: " + input)
    }
    input
  }

  def splitIntoGraphemes(input: String): List[String] = {
    val graphemeRegex = "\\X".r
    val res = graphemeRegex.findAllIn(input).toList
    return res
  }

}

