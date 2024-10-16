package staticFileGenerators.cedictMap

import ElementGenerator.ElementTranslateToAlphabet
import UtilityClasses.{CedictEntry, Grapheme, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes

import scala.collection.mutable

class CedictMapTest extends AnyFlatSpec with Matchers{

  "cedict map" should "have the same size as the cedict set" in {
    val cedict: Set[CedictEntry]  = GenerateCedictMap.cedictCompleteSet
    val cedictMap: Map[String, CedictEntry] = GenerateCedictMap.cedictMap
    cedictMap.size shouldBe cedict.size
  }
  
  "testAllCedict" should "contain only a fraction of the characters in conway" in {
    val cedict: Set[CedictEntry]  = GenerateCedictMap.cedictCompleteSet
    val conwayChars: Set[Grapheme] = GenerateConwayCodes.conwaySet

    var conwayMissingFromCedict: mutable.HashSet[Grapheme] = mutable.HashSet[Grapheme]()
    for (entry <- cedict) {
      if (entry.chineseStrGraphemes.size == 1) {
        conwayMissingFromCedict.addAll(entry.chineseStrGraphemes)
      }
    }
    conwayMissingFromCedict.size shouldBe 14607
  }

  "testAllCedict" should "contain given entries" in {
    val cedict: Set[CedictEntry]  = GenerateCedictMap.cedictCompleteSet
    val conwayChars: Set[Grapheme] = GenerateConwayCodes.conwaySet

    val test1: List[CedictEntry] = cedict.filter(x => x.chineseStr == "龟笑鳖无尾").toList
    val test2: List[CedictEntry] = cedict.filter(x => x.chineseStr == "龜笑鱉無尾").toList

    test1.size shouldBe 1
    test2.size shouldBe 1
    cedict.size shouldBe 194386
  }

  "testAllCedict" should "containAllConway" in {
    val cedict: Set[CedictEntry]  = GenerateCedictMap.cedictCompleteSet
    val conwayChars: Set[Grapheme] = GenerateConwayCodes.conwaySet

    var conwayMissingFromCedict: mutable.HashSet[Grapheme] = mutable.HashSet[Grapheme]()
    for (eachCedictLine <- cedict) {
      for (eachGrapheme <- eachCedictLine.chineseStrGraphemes) {
        if (!conwayChars.contains(eachGrapheme) 
          && eachGrapheme.char.head > 127
          && !conwayMissingFromCedict.contains(eachGrapheme)) {
          conwayMissingFromCedict.add(eachGrapheme)
        }
      }
    }
    conwayMissingFromCedict.size shouldBe 4
    conwayMissingFromCedict.map(x => x.char).toSet shouldBe Set("○","·","ˋ","π")
  }
  
}
