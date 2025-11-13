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

    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "tbf")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "端子, tbf, CedictPrim:2Cri:5,WordPrim:0020509CharPrim:00916,00037CedictSec:2WordSec:9999999CharSec:01083,00063端子-" +
        "剂子, tbf, CedictPrim:2Cri:5,WordPrim:9999999CharPrim:01546,00037CedictSec:3WordSec:9999999CharSec:99999,00063剂子-" +
        "劑子, tbf, CedictPrim:3Cri:5,WordPrim:9999999CharPrim:99999,00037CedictSec:2WordSec:9999999CharSec:02100,00063劑子-" +
        "剷除, tbf, CedictPrim:3Cri:5,WordPrim:9999999CharPrim:99999,00464CedictSec:2WordSec:9999999CharSec:04078,00444剷除-" +
        "癟陷, tbf, CedictPrim:3Cri:5,WordPrim:9999999CharPrim:99999,01262CedictSec:2WordSec:9999999CharSec:04310,01612癟陷"
  }

  it should "test sorting two letter codes simplified part one" in {

    val se_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "se")

    val allOrdered = se_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "登, se, CedictPrim:2Cri:3,CharPrim:00817WordPrim:0003603CedictSec:2CharSec:00827WordSec:9999999登-" +
        "蚁, se, CedictPrim:2Cri:4,CharPrim:02517WordPrim:0016379CedictSec:3CharSec:99999WordSec:9999999蚁-" +
        "凳, se, CedictPrim:2Cri:4,CharPrim:02680WordPrim:0011101CedictSec:2CharSec:02850WordSec:9999999凳-" +
        "蝉, se, CedictPrim:2Cri:4,CharPrim:03048WordPrim:0018819CedictSec:3CharSec:99999WordSec:9999999蝉-" +
        "蜕, se, CedictPrim:2Cri:4,CharPrim:03624WordPrim:0026643CedictSec:3CharSec:99999WordSec:9999999蜕-" +
        "蜷, se, CedictPrim:2Cri:4,CharPrim:03673WordPrim:0040562CedictSec:2CharSec:05065WordSec:9999999蜷-" +
        "蟮, se, CedictPrim:2Cri:4,CharPrim:04280WordPrim:0012740CedictSec:2CharSec:99999WordSec:9999999蟮-" +
        "癸, se, CedictPrim:2Cri:4,CharPrim:04414WordPrim:0044202CedictSec:2CharSec:03460WordSec:9999999癸-" +
        "鄧, se, CedictPrim:3Cri:3,CharPrim:08177WordPrim:0061199CedictSec:2CharSec:02081WordSec:9999999鄧-" +
        "發, se, CedictPrim:3Cri:3,CharPrim:99999WordPrim:0003650CedictSec:2CharSec:00081WordSec:0003265發-" +
        "蟻, se, CedictPrim:3Cri:4,CharPrim:07672WordPrim:0108801CedictSec:2CharSec:02122WordSec:9999999蟻-" +
        "蛢, se, CedictPrim:3Cri:4,CharPrim:08642WordPrim:9999999CedictSec:3CharSec:04636WordSec:9999999蛢"
  }
  
  

  it should "test sorting two letter codes simplified part two" in {

    val fh_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "fh")

    val allOrdered = fh_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "子, fh, CedictPrim:2Cri:3,CharPrim:00037WordPrim:0000469CedictSec:2CharSec:00063WordSec:9999999子-" +
        "际, fh, CedictPrim:2Cri:3,CharPrim:00423WordPrim:0006291CedictSec:3CharSec:99999WordSec:9999999际-" +
        "承, fh, CedictPrim:2Cri:3,CharPrim:00639WordPrim:0006275CedictSec:2CharSec:00911WordSec:9999999承-" +
        "卫, fh, CedictPrim:2Cri:3,CharPrim:00669WordPrim:0003259CedictSec:3CharSec:99999WordSec:9999999卫-" +
        "陆, fh, CedictPrim:2Cri:3,CharPrim:00675WordPrim:0005086CedictSec:3CharSec:99999WordSec:9999999陆-" +
        "籍, fh, CedictPrim:2Cri:3,CharPrim:01579WordPrim:0005221CedictSec:2CharSec:01208WordSec:9999999籍-" +
        "筹, fh, CedictPrim:2Cri:3,CharPrim:01677WordPrim:0004860CedictSec:3CharSec:99999WordSec:9999999筹-" +
        "丑, fh, CedictPrim:2Cri:3,CharPrim:01901WordPrim:0005012CedictSec:2CharSec:02958WordSec:9999999丑-" +
        "竿, fh, CedictPrim:2Cri:4,CharPrim:02934WordPrim:0022541CedictSec:2CharSec:02446WordSec:9999999竿-" +
        "阮, fh, CedictPrim:2Cri:4,CharPrim:03210WordPrim:0021189CedictSec:2CharSec:02107WordSec:9999999阮-" +
        "阱, fh, CedictPrim:2Cri:4,CharPrim:03224WordPrim:0078324CedictSec:2CharSec:02993WordSec:9999999阱-" +
        "筐, fh, CedictPrim:2Cri:4,CharPrim:03419WordPrim:0016229CedictSec:2CharSec:04196WordSec:9999999筐-" +
        "竺, fh, CedictPrim:2Cri:4,CharPrim:03439WordPrim:0026543CedictSec:2CharSec:03658WordSec:9999999竺-" +
        "笺, fh, CedictPrim:2Cri:4,CharPrim:03731WordPrim:0031903CedictSec:3CharSec:99999WordSec:9999999笺-" +
        "孺, fh, CedictPrim:2Cri:4,CharPrim:04084WordPrim:0050496CedictSec:2CharSec:03888WordSec:9999999孺-" +
        "篝, fh, CedictPrim:2Cri:4,CharPrim:04336WordPrim:0199635CedictSec:2CharSec:06266WordSec:9999999篝-" +
        "孑, fh, CedictPrim:2Cri:4,CharPrim:04691WordPrim:0063222CedictSec:2CharSec:04799WordSec:9999999孑-" +
        "竽, fh, CedictPrim:2Cri:4,CharPrim:04935WordPrim:0048648CedictSec:2CharSec:04041WordSec:9999999竽-" +
        "孒, fh, CedictPrim:3Cri:4,CharPrim:99999WordPrim:0097549CedictSec:3CharSec:99999WordSec:9999999孒"
  }

  it should "test sorting two letters - traditional" in {

    val kg_codes = sorted_sortCharactersTraditional.filter(x => x._2 == "kg")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "眼, kg, CedictPrim:2Cri:3,CharPrim:00494WordPrim:0000920CedictSec:2CharSec:00281WordSec:0000644眼-" +
        "万, kg, CedictPrim:2Cri:3,CharPrim:04033WordPrim:9999999CedictSec:2CharSec:00322WordSec:9999999万-" +
        "眠, kg, CedictPrim:2Cri:4,CharPrim:01434WordPrim:9999999CedictSec:2CharSec:01986WordSec:0010359眠-" +
        "尷, kg, CedictPrim:2Cri:4,CharPrim:02812WordPrim:9999999CedictSec:3CharSec:99999WordSec:0072694尷-" +
        "兀, kg, CedictPrim:2Cri:4,CharPrim:03743WordPrim:9999999CedictSec:2CharSec:02859WordSec:0016338兀-" +
        "尪, kg, CedictPrim:2Cri:4,CharPrim:03892WordPrim:9999999CedictSec:2CharSec:99999WordSec:0355675尪-" +
        "矚, kg, CedictPrim:2Cri:4,CharPrim:03966WordPrim:9999999CedictSec:3CharSec:08107WordSec:0206875矚-" +
        "瞰, kg, CedictPrim:2Cri:4,CharPrim:04278WordPrim:9999999CedictSec:2CharSec:03895WordSec:0078460瞰-" +
        "臧, kg, CedictPrim:2Cri:4,CharPrim:04515WordPrim:9999999CedictSec:2CharSec:04304WordSec:0035493臧-" +
        "尢, kg, CedictPrim:2Cri:4,CharPrim:05865WordPrim:9999999CedictSec:2CharSec:06516WordSec:0074614尢-" +
        "ㄤ, kg, CedictPrim:2Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:99999WordSec:0186175ㄤ-" +
        "兀, kg, CedictPrim:3Cri:4,CharPrim:04782WordPrim:9999999CedictSec:3CharSec:99999WordSec:0293231兀-" +
        "瞩, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:03320WordSec:0032701瞩-" +
        "厩, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:04181WordSec:0037804厩-" +
        "⺎, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺎-" +
        "⺐, kg, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:9999999⺐"
  }
  

  it should "test sorting three letters - traditional" in {

    val kg_codes = sorted_sortCharactersTraditional.filter(x => x._2 == "tbf")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "端子, tbf, CedictPrim:2Cri:5,WordPrim:9999999CharPrim:01083,00063CedictSec:2WordSec:0020509CharSec:00916,00037端子-" +
        "劑子, tbf, CedictPrim:2Cri:5,WordPrim:9999999CharPrim:02100,00063CedictSec:3WordSec:9999999CharSec:99999,00037劑子-" +
        "剷除, tbf, CedictPrim:2Cri:5,WordPrim:9999999CharPrim:04078,00444CedictSec:3WordSec:9999999CharSec:99999,00464剷除-" +
        "癟陷, tbf, CedictPrim:2Cri:5,WordPrim:9999999CharPrim:04310,01612CedictSec:3WordSec:9999999CharSec:99999,01262癟陷-" +
        "剂子, tbf, CedictPrim:3Cri:5,WordPrim:9999999CharPrim:99999,00063CedictSec:2WordSec:9999999CharSec:01546,00037剂子"
  }

  it should "test sorting two letter codes traditional part one" in {

    val se_codes = sorted_sortCharactersTraditional.filter(x => x._2 == "se")

    val allOrdered = se_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "發, se, CedictPrim:2Cri:3,CharPrim:00081WordPrim:0003265CedictSec:3CharSec:99999WordSec:0003650發-" +
        "登, se, CedictPrim:2Cri:3,CharPrim:00827WordPrim:9999999CedictSec:2CharSec:00817WordSec:0003603登-" +
        "鄧, se, CedictPrim:2Cri:3,CharPrim:02081WordPrim:9999999CedictSec:3CharSec:08177WordSec:0061199鄧-" +
        "蟻, se, CedictPrim:2Cri:4,CharPrim:02122WordPrim:9999999CedictSec:3CharSec:07672WordSec:0108801蟻-" +
        "凳, se, CedictPrim:2Cri:4,CharPrim:02850WordPrim:9999999CedictSec:2CharSec:02680WordSec:0011101凳-" +
        "癸, se, CedictPrim:2Cri:4,CharPrim:03460WordPrim:9999999CedictSec:2CharSec:04414WordSec:0044202癸-" +
        "蜷, se, CedictPrim:2Cri:4,CharPrim:05065WordPrim:9999999CedictSec:2CharSec:03673WordSec:0040562蜷-" +
        "蟮, se, CedictPrim:2Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:04280WordSec:0012740蟮-" +
        "蛢, se, CedictPrim:3Cri:4,CharPrim:04636WordPrim:9999999CedictSec:3CharSec:08642WordSec:9999999蛢-" +
        "蚁, se, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:02517WordSec:0016379蚁-" +
        "蝉, se, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:03048WordSec:0018819蝉-" +
        "蜕, se, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:03624WordSec:0026643蜕"
  }

  it should "test sorting two letter codes traditional part two" in {

    val fh_codes = sorted_sortCharactersTraditional.filter(x => x._2 == "fh")

    val allOrdered = fh_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe
      "子, fh, CedictPrim:2Cri:3,CharPrim:00063WordPrim:9999999CedictSec:2CharSec:00037WordSec:0000469子-" +
        "承, fh, CedictPrim:2Cri:3,CharPrim:00911WordPrim:9999999CedictSec:2CharSec:00639WordSec:0006275承-" +
        "籍, fh, CedictPrim:2Cri:3,CharPrim:01208WordPrim:9999999CedictSec:2CharSec:01579WordSec:0005221籍-" +
        "丑, fh, CedictPrim:2Cri:3,CharPrim:02958WordPrim:9999999CedictSec:2CharSec:01901WordSec:0005012丑-" +
        "阮, fh, CedictPrim:2Cri:4,CharPrim:02107WordPrim:9999999CedictSec:2CharSec:03210WordSec:0021189阮-" +
        "竿, fh, CedictPrim:2Cri:4,CharPrim:02446WordPrim:9999999CedictSec:2CharSec:02934WordSec:0022541竿-" +
        "阱, fh, CedictPrim:2Cri:4,CharPrim:02993WordPrim:9999999CedictSec:2CharSec:03224WordSec:0078324阱-" +
        "竺, fh, CedictPrim:2Cri:4,CharPrim:03658WordPrim:9999999CedictSec:2CharSec:03439WordSec:0026543竺-" +
        "孺, fh, CedictPrim:2Cri:4,CharPrim:03888WordPrim:9999999CedictSec:2CharSec:04084WordSec:0050496孺-" +
        "竽, fh, CedictPrim:2Cri:4,CharPrim:04041WordPrim:9999999CedictSec:2CharSec:04935WordSec:0048648竽-" +
        "筐, fh, CedictPrim:2Cri:4,CharPrim:04196WordPrim:9999999CedictSec:2CharSec:03419WordSec:0016229筐-" +
        "孑, fh, CedictPrim:2Cri:4,CharPrim:04799WordPrim:9999999CedictSec:2CharSec:04691WordSec:0063222孑-" +
        "篝, fh, CedictPrim:2Cri:4,CharPrim:06266WordPrim:9999999CedictSec:2CharSec:04336WordSec:0199635篝-" +
        "际, fh, CedictPrim:3Cri:3,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:00423WordSec:0006291际-" +
        "卫, fh, CedictPrim:3Cri:3,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:00669WordSec:0003259卫-" +
        "陆, fh, CedictPrim:3Cri:3,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:00675WordSec:0005086陆-" +
        "筹, fh, CedictPrim:3Cri:3,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:01677WordSec:0004860筹-" +
        "笺, fh, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:2CharSec:03731WordSec:0031903笺-" +
        "孒, fh, CedictPrim:3Cri:4,CharPrim:99999WordPrim:9999999CedictSec:3CharSec:99999WordSec:0097549孒"
  }


}
