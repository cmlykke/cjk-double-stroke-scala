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

    val allCodes: Set[(String, Set[(String, AsortingCriteria)])] =
      AgenerateTranslation.translationsOfSetOfStrings(AsingletonsForTests.chineseTextitems, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation)

    val output: Map[String, Set[(String, String, AsortingCriteria)]] = AsortWordsAndCharacters.convertTranslatedTextToSortFormat(allCodes)

    val test = ""
  }



}
