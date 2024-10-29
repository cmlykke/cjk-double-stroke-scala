package ElementGenerator

import UtilityClasses.{CharSystem, Grapheme, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElementTranslateToAlphabetTest extends AnyFlatSpec with Matchers {

  it should "test that complete translated conway map has expected translations" in {
    //completeTranslatedConwayMap: Map[Grapheme, StaticFileCharInfoWithLetterConway]
    val completeConway: Map[Grapheme, StaticFileCharInfoWithLetterConway] =
      ElementTranslateToAlphabet.completeTranslatedConwayMap
    val  woI: Option[StaticFileCharInfoWithLetterConway] = completeConway.get(Grapheme("我"))
    val codes: Set[String] = woI.get.letterConway.map(x => x.conwayPairs.mkString("")).toSet
    
    codes shouldBe Set("ynsy", "yndt")
  }

  it should "test codes of all elements" in {
    //completeTranslatedConwayMap: Map[Grapheme, StaticFileCharInfoWithLetterConway]
    val completeConway: Map[Grapheme, StaticFileCharInfoWithLetterConway] =
      ElementTranslateToAlphabet.completeTranslatedConwayMap
    val allelements: Set[ElementType] = ElementList.elementTypes
    var charactersAndletter: List[(String, List[String])] = List[(String, List[String])]()
    for (elem <- allelements) {
      if (Grapheme.isGrapheme(elem.rawString)) {
        val woI: StaticFileCharInfoWithLetterConway = completeConway.get(Grapheme(elem.rawString)).get
        val elemCodes: Set[List[String]] = woI.letterConway.map(x => x.conwayPairs).toSet
        val letterIncluded: List[List[String]] = elemCodes.filter(x => (x.size == 1) && (x.head.length == 1)).toList
        charactersAndletter = charactersAndletter.appended((elem.rawString, letterIncluded.head))
      }
    }
    charactersAndletter.size shouldBe 35
  }
  
  it should "junda - check that the map of character to alphabet has the same size" in {
    //createMapFromSet

    val first8000 = ElementTranslateToAlphabet.currentChoice8000Juda
    val alphaMap = ElementTranslateToAlphabet.createMapFromSet(first8000)
    alphaMap.size shouldBe first8000.size

    val woI: Option[StaticFileCharInfoWithLetterConway] = alphaMap.get(Grapheme("我"))
    val codes: Set[String] = woI.get.letterConway.map(x => x.conwayPairs.mkString("")).toSet
    codes shouldBe Set("ynsy", "yndt")
  }
  
  it should "check Junda - frst 8000 chars with firstChoice translate map" in {

    val first8000 = ElementTranslateToAlphabet.currentChoice8000Juda
    val adjusted: ElementAdjustedCodes = new ElementAdjustedCodes()
    val secondOverlapJunda: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
      adjusted.secondOverlapMap(CharSystem.Junda, first8000)

    val jundaUPDATEDformatted = FormatUtils.formatFinalRes(secondOverlapJunda, CharSystem.Junda)

    val snip: String = FormatUtils.summarizeOverlap(secondOverlapJunda)
    snip shouldBe "First 3 keys: [5105, 5209, 5772], Total keys: 29, Total beyond index 8: 104"

    val test = ""
  }


  it should "tzai- check that the map of character to alphabet has the same size" in {
    //createMapFromSet

    val first8000 = ElementTranslateToAlphabet.currentChoice8000Tzai
    val alphaMap = ElementTranslateToAlphabet.createMapFromSet(first8000)
    alphaMap.size shouldBe first8000.size
    
    val woI: Option[StaticFileCharInfoWithLetterConway] = alphaMap.get(Grapheme("我"))
    val codes: Set[String] = woI.get.letterConway.map(x => x.conwayPairs.mkString("")).toSet
    codes shouldBe Set("ynsy", "yndt")
  }

  it should "check Tzai - frst 8000 chars with firstChoice translate map" in {
    val first8000 = ElementTranslateToAlphabet.currentChoice8000Tzai
    val adjusted: ElementAdjustedCodes = new ElementAdjustedCodes()
    val secondOverlapJunda: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
      adjusted.secondOverlapMap(CharSystem.Tzai, first8000)

    val jundaUPDATEDformatted = FormatUtils.formatFinalRes(secondOverlapJunda, CharSystem.Tzai)

    val snip: String = FormatUtils.summarizeOverlap(secondOverlapJunda)

    snip shouldBe "First 3 keys: [5168, 5168, 5412], Total keys: 22, Total beyond index 8: 78"

    val test = ""
  }
  
}
