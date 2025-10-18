package AcodeGenerator

import AcodeGenerators.AgenerateTranslation
import Adatasources.FileReaders.{AidsData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Atypes.SortingCodes.{FiveCode, FourCode, SixCode, ThreeCode, TwoCode}
import Atypes.{Aelementstype, Agrapheme, SortingCodes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateTranslationTest extends AnyFlatSpec with Matchers {

  val conwaymap = AreadConwayData.mapConwayData()
  val idsmap = AidsData.idsDataRaw()
  val idsToStrokeMap: Map[String, Aelementstype] = Aelements.idsToStrokeMap
  val basicTranslation: Map[String, String] = AcodelengthRules.elementTypes

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
















