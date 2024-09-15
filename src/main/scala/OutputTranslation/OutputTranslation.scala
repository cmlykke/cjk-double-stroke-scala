package OutputTranslation

import UtilityClasses.{CedictEntry, ConwayUnambigous, Grapheme, OutputEntry}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable

type TranslationFunction = List[Set[ConwayUnambigous]] => Set[String]

class OutputTranslation {
  def generateJundaGraphemeOrder(chineseStrGraphemes: Set[Grapheme]): List[Grapheme] = {
    // Define an ordering based on the required sorting criteria
    val ordering: Ordering[Grapheme] = Ordering.by(grapheme => {
      def extractHexValue(hexString: String): Int = {
        Integer.parseInt(hexString.stripPrefix("U+"), 16)
      }

      // Ensure we use a very low frequency if not present to avoid being placed before entries with data
      val jundaFreq = -grapheme.junda.map(_.frequency).getOrElse(Double.MinValue)
      val tzaiFreq = -grapheme.tzai.map(_.frequency).getOrElse(Double.MinValue)
      val unicodeLen = grapheme.unicode.length
      val lastHexValue = grapheme.unicode.lastOption.map(extractHexValue).getOrElse(Int.MaxValue)
      (jundaFreq, tzaiFreq, unicodeLen, lastHexValue)
    })
    // Convert the set to a list and sort using the defined ordering
    chineseStrGraphemes.toList.sorted(ordering)
  }

  def generateTzaiGraphemeOrder(chineseStrGraphemes: Set[Grapheme]): List[Grapheme] = {
    // Define an ordering based on the required sorting criteria
    val ordering: Ordering[Grapheme] = Ordering.by(grapheme => {
      def extractHexValue(hexString: String): Int = {
        Integer.parseInt(hexString.stripPrefix("U+"), 16)
      }

      // Ensure we use a very low frequency if not present to avoid being placed before entries with data
      val tzaiFreq = -grapheme.tzai.map(_.frequency).getOrElse(Double.MinValue)
      val jundaFreq = -grapheme.junda.map(_.frequency).getOrElse(Double.MinValue)
      val unicodeLen = grapheme.unicode.length
      val lastHexValue = grapheme.unicode.lastOption.map(extractHexValue).getOrElse(Int.MaxValue)
      (tzaiFreq, jundaFreq, unicodeLen, lastHexValue)
    })
    // Convert the set to a list and sort using the defined ordering
    chineseStrGraphemes.toList.sorted(ordering)
  }

  def cedictToOutputEntry(cedictEntries: Set[CedictEntry], 
                          translationFn: TranslationFunction): Set[OutputEntry] = {
    val res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (ceEntry <- cedictEntries) {
      if (ceEntry.chineseStr == "井陘礦") {
        val tes: String = ""
      }
      val jundaOrder: List[Grapheme] = generateJundaGraphemeOrder(ceEntry.chineseStrGraphemes)
      val tzaiOrder: List[Grapheme] = generateTzaiGraphemeOrder(ceEntry.chineseStrGraphemes)
      val outputCodes: Set[String] = translationFn(ceEntry.unambigous)
      val entry: OutputEntry = new OutputEntry(
        ceEntry.chineseStr,
        ceEntry.meaning,
        ceEntry.pronounciation,
        ceEntry.tradSimp,
        jundaOrder,
        tzaiOrder,
        outputCodes
      )
      res.add(entry)
    }
    res.toSet
  }

}

object OutputTranslation {
  val outClass = new OutputTranslation()
  val cedict: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val conwayChars: Set[Grapheme] = GenerateConwayCodes.conwaySet
  val outputCedict: Set[OutputEntry] = outClass.cedictToOutputEntry(cedict, TranslationFunctions.translateVersionOne)

}
