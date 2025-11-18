package AsortingTests

//import AcodeGenerators.{AredoneTranslation, AsortedOutput}
//import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
//import Adatasources.ManualData.{AcodelengthRules, Aelements, AtextType}
//import Asingletons.AsingletonsForTestsd
//import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, AsortingObject, PossibleWordCodes, SortingCodes}
import ApublishingCodes.AgenerateOutputStrings
import Asingletons.AsingletonsForTests
import Atypes.AsortingObject
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
      "眼, kg, CedictPrim:2Cri:3,CharPrim:00281WordPrim:0000644CedictSec:2CharSec:00494WordSec:0000920眼-" +
        "万, kg, CedictPrim:2Cri:3,CharPrim:00322WordPrim:9999999CedictSec:2CharSec:04033WordSec:9999999万-" +
        "眠, kg, CedictPrim:2Cri:4,CharPrim:01986WordPrim:0010359CedictSec:2CharSec:01434WordSec:9999999眠-" +
        "兀, kg, CedictPrim:2Cri:4,CharPrim:02859WordPrim:0016338CedictSec:2CharSec:03743WordSec:9999999兀-" +
        "瞩, kg, CedictPrim:2Cri:4,CharPrim:03320WordPrim:0032701CedictSec:3CharSec:99999WordSec:9999999瞩-" +
        "瞰, kg, CedictPrim:2Cri:4,CharPrim:03895WordPrim:0078460CedictSec:2CharSec:04278WordSec:9999999瞰-" +
        "厩, kg, CedictPrim:2Cri:4,CharPrim:04181WordPrim:0037804CedictSec:3CharSec:99999WordSec:9999999厩-" +
        "臧, kg, CedictPrim:2Cri:4,CharPrim:04304WordPrim:0035493CedictSec:2CharSec:04515WordSec:9999999臧-" +
        "尢, kg, CedictPrim:2Cri:4,CharPrim:06516WordPrim:0074614CedictSec:2CharSec:05865WordSec:9999999尢-" +
        "ㄤ, kg, CedictPrim:2Cri:4,CharPrim:99999WordPrim:0186175CedictSec:2CharSec:99999WordSec:9999999ㄤ-" +
        "尪, kg, CedictPrim:2Cri:4,CharPrim:99999WordPrim:0355675CedictSec:2CharSec:03892WordSec:9999999尪-" +
        "矚, kg, CedictPrim:3Cri:4,CharPrim:08107WordPrim:0206875CedictSec:2CharSec:03966WordSec:9999999矚-" +
        "尷, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:0072694CedictSec:2CharSec:02812WordSec:9999999尷-" +
        "兀, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:0293231CedictSec:3CharSec:04782WordSec:9999999兀-" +
        "⺎, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺎-" +
        "⺐, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺐"
  }


  it should "test sorting three letters" in {

    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "hst").take(10)

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "适应, hst, CedictPrim:2Cri:5,WordPrim:0001750CharPrim:00663,00144CedictSec:3WordSec:9999999CharSec:99999,06603适应-" +
        "武装, hst, CedictPrim:2Cri:5,WordPrim:0002586CharPrim:00501,00467CedictSec:3WordSec:9999999CharSec:99999,00659武装-" +
        "远离, hst, CedictPrim:2Cri:5,WordPrim:0005301CharPrim:00418,00386CedictSec:3WordSec:9999999CharSec:99999,07102远离-" +
        "进度, hst, CedictPrim:2Cri:5,WordPrim:0006697CharPrim:00184,00081CedictSec:3WordSec:9999999CharSec:99999,00259进度-" +
        "适度, hst, CedictPrim:2Cri:5,WordPrim:0007885CharPrim:00663,00184CedictSec:2WordSec:9999999CharSec:06603,00259适度-" +
        "违章, hst, CedictPrim:2Cri:5,WordPrim:0009073CharPrim:01184,00539CedictSec:3WordSec:9999999CharSec:99999,00079违章-" +
        "远方, hst, CedictPrim:2Cri:5,WordPrim:0009196CharPrim:00386,00060CedictSec:3WordSec:9999999CharSec:99999,00108远方-" +
        "进站, hst, CedictPrim:2Cri:5,WordPrim:0014989CharPrim:00544,00081CedictSec:3WordSec:9999999CharSec:99999,00067进站-" +
        "进京, hst, CedictPrim:2Cri:5,WordPrim:0016211CharPrim:00566,00081CedictSec:3WordSec:9999999CharSec:99999,01339进京-" +
        "武将, hst, CedictPrim:2Cri:5,WordPrim:0017008CharPrim:00501,00132CedictSec:3WordSec:9999999CharSec:99999,00659武将"
  }

  it should "test sorting three letters - code uod" in {
    
    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "uod")

    val allOrdered = kg_codes.take(10).map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "魅力, uod, CedictPrim:2Cri:5,WordPrim:0003089CharPrim:02662,00106CedictSec:2WordSec:0003058CharSec:01983,00141魅力-" +
        "你好, uod, CedictPrim:2Cri:5,WordPrim:0003460CharPrim:00082,00032CedictSec:2WordSec:9999999CharSec:00021,00019你好-" +
        "做梦, uod, CedictPrim:2Cri:5,WordPrim:0006932CharPrim:00865,00246CedictSec:3WordSec:9999999CharSec:99999,00205做梦-" +
        "侦查, uod, CedictPrim:2Cri:5,WordPrim:0007050CharPrim:01479,00459CedictSec:3WordSec:9999999CharSec:99999,00614侦查-" +
        "保姆, uod, CedictPrim:2Cri:5,WordPrim:0008557CharPrim:01061,00266CedictSec:2WordSec:9999999CharSec:01730,00372保姆-" +
        "兵力, uod, CedictPrim:2Cri:5,WordPrim:0008842CharPrim:00398,00106CedictSec:2WordSec:9999999CharSec:00685,00141兵力-" +
        "债权, uod, CedictPrim:2Cri:5,WordPrim:0009397CharPrim:01223,00297CedictSec:3WordSec:9999999CharSec:99999,99999债权-" +
        "货架, uod, CedictPrim:2Cri:5,WordPrim:0021283CharPrim:00846,00818CedictSec:3WordSec:9999999CharSec:99999,00976货架-" +
        "兵刃, uod, CedictPrim:2Cri:5,WordPrim:0022092CharPrim:02700,00398CedictSec:2WordSec:9999999CharSec:02917,00685兵刃-" +
        "像样, uod, CedictPrim:2Cri:5,WordPrim:0022374CharPrim:00294,00088CedictSec:3WordSec:9999999CharSec:99999,00148像样"
  }


}
