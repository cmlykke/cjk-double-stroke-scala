package OutputTranslation

import UtilityClasses.{CedictEntry, Grapheme, OutputEntry}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap

class OutputSorting {
  
}

object OutputSorting {
  val outClass = new OutputTranslation()
  val cedictSet: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val cedictSetOut: Set[OutputEntry] = OutputTranslation.outputCedict
  val conwayOutSansCedict: Set[OutputEntry] = OutputTranslation.conwayOutSansCedict
  
  //val conway: Set[OutputEntry] =  OutputTranslation.outputConway//GenerateConwayCodes.conwaySet
}
