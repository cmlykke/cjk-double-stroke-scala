package ElementGenerator

import UtilityClasses.{CharSystem, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElementTranslateToAlphabetTest extends AnyFlatSpec with Matchers {

  
  it should "junda - check that the map of character to alphabet has the same size" in {
    //createMapFromSet

    val first8000 = ElementTranslateToAlphabet.currentChoice8000Juda
    val toAlphabet = new ElementTranslateToAlphabet()
    val alphaMap = toAlphabet.createMapFromSet(first8000)
    alphaMap.size shouldBe first8000.size
  }
  
  it should "check Junda - frst 8000 chars with firstChoice translate map" in {

    val first8000 = ElementTranslateToAlphabet.currentChoice8000Juda
    val adjusted: ElementAdjustedCodes = new ElementAdjustedCodes()
    val secondOverlapJunda: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
      adjusted.secondOverlapMap(CharSystem.Junda, first8000)

    val jundaUPDATEDformatted = FormatUtils.formatFinalRes(secondOverlapJunda, CharSystem.Junda)

    val snip: String = FormatUtils.summarizeOverlap(secondOverlapJunda)

    snip shouldBe "First 3 keys: [5105, 5209, 5772], Total keys: 30, Total beyond index 8: 105"

    val test = ""
  }


  it should "tzai- check that the map of character to alphabet has the same size" in {
    //createMapFromSet

    val first8000 = ElementTranslateToAlphabet.currentChoice8000Tzai
    val toAlphabet = new ElementTranslateToAlphabet()
    val alphaMap = toAlphabet.createMapFromSet(first8000)
    alphaMap.size shouldBe first8000.size
  }

  it should "check Tzai - frst 8000 chars with firstChoice translate map" in {
    val first8000 = ElementTranslateToAlphabet.currentChoice8000Tzai
    val adjusted: ElementAdjustedCodes = new ElementAdjustedCodes()
    val secondOverlapJunda: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
      adjusted.secondOverlapMap(CharSystem.Tzai, first8000)

    val jundaUPDATEDformatted = FormatUtils.formatFinalRes(secondOverlapJunda, CharSystem.Tzai)

    val snip: String = FormatUtils.summarizeOverlap(secondOverlapJunda)

    snip shouldBe "First 3 keys: [5168, 5168, 5412], Total keys: 24, Total beyond index 8: 86"

    val test = ""
  }

  
}
