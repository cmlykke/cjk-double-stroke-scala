package AsortingTests

import AcodeGenerators.{AredoneTranslation, AsortedOutput}
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements, AtextType}
import Asingletons.AsingletonsForTests
import AsortingCodes.AsortWordsAndCharacters
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
      "万, kg, Cri:2,CedictPrim:2CharPrim:00322WordPrim:9999999CedictSec:2CharSec:04033WordSec:9999999万-" +
        "兀, kg, Cri:2,CedictPrim:2CharPrim:02859WordPrim:0016338CedictSec:2CharSec:03743WordSec:9999999兀-" +
        "尢, kg, Cri:2,CedictPrim:2CharPrim:06516WordPrim:0074614CedictSec:2CharSec:05865WordSec:9999999尢-" +
        "ㄤ, kg, Cri:2,CedictPrim:2CharPrim:99999WordPrim:0186175CedictSec:2CharSec:99999WordSec:9999999ㄤ-" +
        "兀, kg, Cri:2,CedictPrim:3CharPrim:99999WordPrim:0293231CedictSec:3CharSec:04782WordSec:9999999兀-" +
        "⺎, kg, Cri:2,CedictPrim:3CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺎-" +
        "⺐, kg, Cri:2,CedictPrim:3CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺐"
  }

  it should "test sorting three letters" in {

    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "hst")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "适应, hst, Cri:3,CedictPrim:2WordPrim:0001750CharPrim:00144,00663CedictSec:3WordSec:9999999CharSec:06603,99999适应-" +
        "武装, hst, Cri:3,CedictPrim:2WordPrim:0002586CharPrim:00467,00501CedictSec:3WordSec:9999999CharSec:00659,99999武装-" +
        "远离, hst, Cri:3,CedictPrim:2WordPrim:0005301CharPrim:00386,00418CedictSec:3WordSec:9999999CharSec:07102,99999远离-" +
        "进度, hst, Cri:3,CedictPrim:2WordPrim:0006697CharPrim:00081,00184CedictSec:3WordSec:9999999CharSec:00259,99999进度-" +
        "适度, hst, Cri:3,CedictPrim:2WordPrim:0007885CharPrim:00184,00663CedictSec:2WordSec:9999999CharSec:00259,06603适度-" +
        "违章, hst, Cri:3,CedictPrim:2WordPrim:0009073CharPrim:00539,01184CedictSec:3WordSec:9999999CharSec:00079,99999违章-" +
        "远方, hst, Cri:3,CedictPrim:2WordPrim:0009196CharPrim:00060,00386CedictSec:3WordSec:9999999CharSec:00108,99999远方-" +
        "进站, hst, Cri:3,CedictPrim:2WordPrim:0014989CharPrim:00081,00544CedictSec:3WordSec:9999999CharSec:00067,99999进站-" +
        "进京, hst, Cri:3,CedictPrim:2WordPrim:0016211CharPrim:00081,00566CedictSec:3WordSec:9999999CharSec:01339,99999进京-" +
        "武将, hst, Cri:3,CedictPrim:2WordPrim:0017008CharPrim:00132,00501CedictSec:3WordSec:9999999CharSec:00659,99999武将-" +
        "瑕疵, hst, Cri:3,CedictPrim:2WordPrim:0018032CharPrim:03842,03986CedictSec:2WordSec:9999999CharSec:03004,03107瑕疵-" +
        "远端, hst, Cri:3,CedictPrim:2WordPrim:0018718CharPrim:00386,00916CedictSec:3WordSec:9999999CharSec:01083,99999远端-" +
        "云端, hst, Cri:3,CedictPrim:2WordPrim:0031400CharPrim:00692,00916CedictSec:2WordSec:9999999CharSec:01083,01799云端-" +
        "远望, hst, Cri:3,CedictPrim:2WordPrim:0032604CharPrim:00326,00386CedictSec:3WordSec:9999999CharSec:00252,99999远望-" +
        "远郊, hst, Cri:3,CedictPrim:2WordPrim:0033035CharPrim:00386,02351CedictSec:3WordSec:9999999CharSec:02695,99999远郊-" +
        "迂腐, hst, Cri:3,CedictPrim:2WordPrim:0043340CharPrim:01576,02959CedictSec:2WordSec:9999999CharSec:02060,03750迂腐-" +
        "武库, hst, Cri:3,CedictPrim:2WordPrim:0055542CharPrim:00501,01097CedictSec:3WordSec:9999999CharSec:00659,99999武库-" +
        "远亲, hst, Cri:3,CedictPrim:2WordPrim:0066821CharPrim:00362,00386CedictSec:3WordSec:9999999CharSec:99999,99999远亲-" +
        "适意, hst, Cri:3,CedictPrim:2WordPrim:0091502CharPrim:00104,00663CedictSec:2WordSec:9999999CharSec:00100,06603适意-" +
        "专意, hst, Cri:3,CedictPrim:2WordPrim:0123141CharPrim:00104,00485CedictSec:3WordSec:9999999CharSec:00100,99999专意-" +
        "迂磨, hst, Cri:3,CedictPrim:2WordPrim:0905723CharPrim:01537,02959CedictSec:2WordSec:9999999CharSec:01724,03750迂磨-" +
        "运将, hst, Cri:3,CedictPrim:2WordPrim:9999999CharPrim:00132,00345CedictSec:3WordSec:9999999CharSec:99999,99999运将-" +
        "运交, hst, Cri:3,CedictPrim:2WordPrim:9999999CharPrim:00320,00345CedictSec:3WordSec:9999999CharSec:00027,99999运交-" +
        "运庆, hst, Cri:3,CedictPrim:2WordPrim:9999999CharPrim:00345,01269CedictSec:3WordSec:9999999CharSec:99999,99999运庆-" +
        "祛痰, hst, Cri:3,CedictPrim:2WordPrim:9999999CharPrim:03044,04176CedictSec:2WordSec:9999999CharSec:04211,06019祛痰-" +
        "祛瘀, hst, Cri:3,CedictPrim:2WordPrim:9999999CharPrim:04157,04176CedictSec:2WordSec:9999999CharSec:03860,06019祛瘀-" +
        "武庫, hst, Cri:3,CedictPrim:3WordPrim:9999999CharPrim:00501,99999CedictSec:2WordSec:9999999CharSec:00659,01296武庫-" +
        "遨遊, hst, Cri:3,CedictPrim:3WordPrim:9999999CharPrim:04895,07362CedictSec:2WordSec:9999999CharSec:00596,02434遨遊-" +
        "丟棄, hst, Cri:3,CedictPrim:3WordPrim:9999999CharPrim:99999,99999CedictSec:2WordSec:9999999CharSec:01028,01105丟棄"
  }




}
