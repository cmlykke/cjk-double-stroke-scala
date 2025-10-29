package AsortingTests

import AcodeGenerators.AgenerateTranslation
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Asingletons.AsingletonsForTests
import AsortingCodes.AsortWordsAndCharacters
import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}
import GenerateOutput.GenerateOutputStrings
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*


class AtestSorting extends AnyFlatSpec with Matchers {
  
  it should "test sortingWorks" in {

    val sinplifiedCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.simplifiedWords ++ AsingletonsForTests.cedict.simplifiedAllHanItems
    val traditionalCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.traditionalWords ++ AsingletonsForTests.cedict.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = AsingletonsForTests.conwaymap.map(x => x._1.char).toSet

    val allChineseStr: Set[String] = total ++ conwayStrings

    val allToSingle: Set[String] = allChineseStr.map(x => wordToSingle(x)).flatten
    
    val allCodes: Set[(String, Set[(String, AsortingCriteria)])] =
      AgenerateTranslation.translationsOfSetOfStrings(allChineseStr, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation)

    val output: Map[String, Set[(String, String, AsortingCriteria)]] = AsortWordsAndCharacters.convertTranslatedTextToSortFormat(allCodes)
    
    val test = ""
  }


  private def wordToSingle(input: String): Set[String] = {
    input
      .codePoints()
      .mapToObj(cp => new String(Character.toChars(cp)))
      .collect(java.util.stream.Collectors.toSet())
      .asScala
      .toSet
  }


}
