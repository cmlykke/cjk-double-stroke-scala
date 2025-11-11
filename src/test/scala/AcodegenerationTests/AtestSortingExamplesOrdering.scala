package AcodegenerationTests

import AcodeGenerators.{AredoneTranslation, AsortedOutput}
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements, AtextType}
import Asingletons.AsingletonsForTests
import ApublishingCodes.AsortWordsAndCharacters
import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, AsortingObject, PossibleWordCodes, SortingCodes}
import GenerateOutput.GenerateOutputStrings
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*

class AtestSortingExamplesOrdering extends AnyFlatSpec with Matchers {

  val sorted_sortCharactersSimplified: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedSimp
  val sorted_sortCharactersTraditional: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedTrad

  it should "test sorting two letters" in {

    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "kg")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "万, kg, Cri:3,CedictPrim:2CharPrim:00322WordPrim:9999999CedictSec:2CharSec:04033WordSec:9999999万-" +
        "兀, kg, Cri:3,CedictPrim:2CharPrim:02859WordPrim:0016338CedictSec:2CharSec:03743WordSec:9999999兀-" +
        "尢, kg, Cri:3,CedictPrim:2CharPrim:06516WordPrim:0074614CedictSec:2CharSec:05865WordSec:9999999尢-" +
        "ㄤ, kg, Cri:3,CedictPrim:2CharPrim:99999WordPrim:0186175CedictSec:2CharSec:99999WordSec:9999999ㄤ-" +
        "兀, kg, Cri:3,CedictPrim:3CharPrim:99999WordPrim:0293231CedictSec:3CharSec:04782WordSec:9999999兀-" +
        "⺎, kg, Cri:3,CedictPrim:3CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺎-" +
        "⺐, kg, Cri:3,CedictPrim:3CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺐"
  }

  it should "test sorting three letters" in {

    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "tbf")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "端子, tbf, Cri:4,CedictPrim:2WordPrim:0020509CharPrim:00916,00037CedictSec:2WordSec:9999999CharSec:01083,00063端子-" +
        "剂子, tbf, Cri:4,CedictPrim:2WordPrim:9999999CharPrim:01546,00037CedictSec:3WordSec:9999999CharSec:99999,00063剂子-" +
        "劑子, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:99999,00037CedictSec:2WordSec:9999999CharSec:02100,00063劑子-" +
        "剷除, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:99999,00464CedictSec:2WordSec:9999999CharSec:04078,00444剷除-" +
        "癟陷, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:99999,01262CedictSec:2WordSec:9999999CharSec:04310,01612癟陷"
  }



  it should "test sorting two letters - traditional" in {

    val kg_codes = sorted_sortCharactersTraditional.filter(x => x._2 == "kg")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "兀, kg, Cri:3,CedictPrim:2CharPrim:03743WordPrim:9999999CedictSec:2CharSec:02859WordSec:0016338兀-" +
        "万, kg, Cri:3,CedictPrim:2CharPrim:04033WordPrim:9999999CedictSec:2CharSec:00322WordSec:9999999万-" +
        "尢, kg, Cri:3,CedictPrim:2CharPrim:05865WordPrim:9999999CedictSec:2CharSec:06516WordSec:0074614尢-" +
        "ㄤ, kg, Cri:3,CedictPrim:2CharPrim:99999WordPrim:9999999CedictSec:2CharSec:99999WordSec:0186175ㄤ-" +
        "兀, kg, Cri:3,CedictPrim:3CharPrim:04782WordPrim:9999999CedictSec:3CharSec:99999WordSec:0293231兀-" +
        "⺎, kg, Cri:3,CedictPrim:3CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺎-" +
        "⺐, kg, Cri:3,CedictPrim:3CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺐"
  }

  it should "test sorting three letters - traditional" in {

    val kg_codes = sorted_sortCharactersTraditional.filter(x => x._2 == "tbf")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "端子, tbf, Cri:4,CedictPrim:2WordPrim:9999999CharPrim:01083,00063CedictSec:2WordSec:0020509CharSec:00916,00037端子-" +
        "劑子, tbf, Cri:4,CedictPrim:2WordPrim:9999999CharPrim:02100,00063CedictSec:3WordSec:9999999CharSec:99999,00037劑子-" +
        "剷除, tbf, Cri:4,CedictPrim:2WordPrim:9999999CharPrim:04078,00444CedictSec:3WordSec:9999999CharSec:99999,00464剷除-" +
        "癟陷, tbf, Cri:4,CedictPrim:2WordPrim:9999999CharPrim:04310,01612CedictSec:3WordSec:9999999CharSec:99999,01262癟陷-" +
        "剂子, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:99999,00063CedictSec:2WordSec:9999999CharSec:01546,00037剂子"
  }





}
