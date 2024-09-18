package OutputTranslation

import UtilityClasses.{CedictEntry, Grapheme}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap

class OutputSorting {

}

object OutputSorting {
  val outClass = new OutputTranslation()
  val cedict: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val conwayChars: Set[Grapheme] = GenerateConwayCodes.conwaySet
}
