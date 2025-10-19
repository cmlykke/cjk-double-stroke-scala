package AcodeGenerator

import AcodeGenerators.AgenerateTranslation
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Atypes.SortingCodes.{FiveCode, FourCode, SixCode, ThreeCode, TwoCode}
import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, SortingCodes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateTranslationTest extends AnyFlatSpec with Matchers {

  val conwaymap = AreadConwayData.mapConwayData()
  val idsmap = AidsData.idsDataRaw()
  val idsToStrokeMap: Map[String, Aelementstype] = Aelements.idsToStrokeMap
  val basicTranslation: Map[String, String] = AcodelengthRules.elementTypes

  val coll: AcedictColl = AreadCedictData.listCedictData()

/*
  it should "test that all characters can be read and translated" in {

    val sinplifiedCharsAndWords: Set[AcedictEntry] = coll.simplifiedWords ++ coll.simplifiedAllHanItems

    val traditionalCharsAndWords: Set[AcedictEntry]  = coll.traditionalWords ++ coll.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = conwaymap.map(x => x._1.char).toSet

    val notFoundInConway: Set[String] = total
      .filter(x => x.codePoints().count() == 1)
      .filter( xString => !conwayStrings.contains(xString))

    val notFoundInCedict: Set[String] = conwayStrings.filter(x => !total.contains(x))

    notFoundInConway shouldBe Set("π", "·", "ㄏ", "ˋ")
    notFoundInCedict.size shouldBe 13696
    
  }
  */

  /*
  it should "generateCodesForAllCharacters" in {
    val sinplifiedCharsAndWords: Set[AcedictEntry] = coll.simplifiedWords ++ coll.simplifiedAllHanItems
    val traditionalCharsAndWords: Set[AcedictEntry]  = coll.traditionalWords ++ coll.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = conwaymap.map(x => x._1.char).toSet

    val allChineseStr: Set[String] = total ++ conwayStrings
    
    val allCodes: Set[Set[(String, AsortingCriteria)]] = 
      AgenerateTranslation.translationsOfSetOfStrings(allChineseStr, conwaymap, idsmap, idsToStrokeMap, basicTranslation)
  
    val test = ""
  }
  */
  
  it should "test that single char can be translated" in {

    val test1 = AgenerateTranslation.getTranslationFromChineseString(
      "誠", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test1 shouldBe Set(
      ("ikao", FourCode),
      ("ikae", FourCode),
      ("hhxhpo", SixCode),
      ("hhxhpe", SixCode),
      ("thxhpo", SixCode),
      ("thxhpe", SixCode))

    val test2 = AgenerateTranslation.getTranslationFromChineseString(
      "子", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test2 shouldBe Set(
      ("fh", TwoCode),
      ("fhzz", FourCode),
      ("fhzzzz", SixCode))
  }

  it should "test that a word can be translated" in {

    val test1 = AgenerateTranslation.getTranslationFromChineseString(
      "摳腳", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test1 shouldBe Set(("lmphf", FiveCode), ("lmptf", FiveCode), ("lmp", ThreeCode))
  }

}
















