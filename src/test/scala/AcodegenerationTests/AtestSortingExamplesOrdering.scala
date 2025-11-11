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
      "端子, tbf, Cri:4,CedictPrim:2WordPrim:0020509CharPrim:00037,00916CedictSec:2WordSec:9999999CharSec:00063,01083端子-" +
        "剂子, tbf, Cri:4,CedictPrim:2WordPrim:9999999CharPrim:00037,01546CedictSec:3WordSec:9999999CharSec:00063,99999剂子-" +
        "劑子, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:00037,99999CedictSec:2WordSec:9999999CharSec:00063,02100劑子-" +
        "剷除, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:00464,99999CedictSec:2WordSec:9999999CharSec:00444,04078剷除-" +
        "癟陷, tbf, Cri:4,CedictPrim:3WordPrim:9999999CharPrim:01262,99999CedictSec:2WordSec:9999999CharSec:01612,04310癟陷"
  }




}
