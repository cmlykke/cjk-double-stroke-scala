package staticFileGenerators.cedictMap

import ElementGenerator.ElementTranslateToAlphabet
import UtilityClasses.{CedictEntry, Grapheme, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes

import scala.collection.mutable

class cedictMapTest extends AnyFlatSpec with Matchers{

  "testAllCedict" should "containAllConway" in {
    val cedict: Set[CedictEntry]  = GenerateCedictMap.cedictCompleteSet
    val conwayChars: Set[Grapheme] = GenerateConwayCodes.conwaySet
    conwayChars.size == 28095

    var conwayMissingFromCedict: mutable.HashSet[Grapheme] = mutable.HashSet[Grapheme]()
    for (eachCedictLine <- cedict) {
      for (eachGrapheme <- eachCedictLine.chineseStrGraphemes) {
        if (!conwayChars.contains(eachGrapheme) 
          && eachGrapheme.char.head > 1000
          && !conwayMissingFromCedict.contains(eachGrapheme)) {
          conwayMissingFromCedict.add(eachGrapheme)
        }
      }
    }

    val test = ""
    /*
    val idsFilePath = "src/main/scala/staticFileGenerators/staticFiles/cedict_ts.u8" // replace with your actual file path
    val radicalSupplement = "src/main/scala/staticFileGenerators/staticFiles/radicals1.txt"

    val tras = new ElementTranslateToAlphabet() //ElementTranslateToAlphabet
    val allch: Set[StaticFileCharInfoWithLetterConway] = tras.generateTranslatedAllChars() //generateTranslatedAllChars   ElementTranslateToAlphabet
    val trans2: Map[Grapheme, StaticFileCharInfoWithLetterConway] = tras.createMapFromSet(allch)

    val res = tes.generateCedictFromFile(idsFilePath, trans2)

    val fin = ""*/
  }


  
}
