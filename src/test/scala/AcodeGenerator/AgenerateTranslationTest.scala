package AcodeGenerator

import AcodeGenerators.AgenerateTranslation
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Atypes.SortingCodes.{FiveCode, FourCode, SixCode, ThreeCode, TwoCode}
import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}
import GenerateOutput.GenerateOutputStrings
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*


class AgenerateTranslationTest extends AnyFlatSpec with Matchers {

  val conwaymap = AreadConwayData.mapConwayData()
  val idsmap = AidsData.idsDataRaw()
  val idsToStrokeMap: Map[String, Aelementstype] = Aelements.idsToStrokeMap
  val basicTranslation: Map[String, String] = AcodelengthRules.elementTypes

  val coll: AcedictColl = AreadCedictData.listCedictData()

  it should "generateCodesForAllCharacters" in {
    val sinplifiedCharsAndWords: Set[AcedictEntry] = coll.simplifiedWords ++ coll.simplifiedAllHanItems
    val traditionalCharsAndWords: Set[AcedictEntry] = coll.traditionalWords ++ coll.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = conwaymap.map(x => x._1.char).toSet

    val allChineseStr: Set[String] = total ++ conwayStrings

    val allToSingle: Set[String] = allChineseStr.map(x => wordToSingle(x)).flatten

    val notFoundConway: List[String] = allToSingle.filter(x => !conwaymap.contains(Agrapheme(x))).toList.sorted
    val notFoundIds: List[String] = allToSingle.filter(x => !idsmap.contains(Agrapheme(x))).toList.sorted


    //new codes
    val allCodes: Set[(String, Set[(String, AsortingCriteria)])] =
      AgenerateTranslation.translationsOfSetOfStrings(allChineseStr, conwaymap, idsmap, idsToStrokeMap, basicTranslation)

    val allNewCodes_unsortedStrings: Set[(String, String)] = allCodes.map { x =>
      x._2.map { y => (x._1, y._1) }
    }.flatten.toSet

    val oldCodes: SortedMap[String, List[OutputEntry]] = GenerateOutputStrings.mapFullJunda
    val oldCodes_unsortedStrings: Set[(String, String)] =
      oldCodes.values.flatten.toSet.map(x => (x.codes.map{y => (x.chineseStr, y)})).flatten.toSet

    val fbwtest1 = oldCodes.get("fbw")
    val fbwtest2 = oldCodes.get("fbwkt")
    val fbwtest3 = oldCodes_unsortedStrings.filter(x => x._1 == "箭头")
    
    val NewNotInOld: Set[(String, String)] = allNewCodes_unsortedStrings.diff(oldCodes_unsortedStrings)
    val OldNotInNew: Set[(String, String)] = oldCodes_unsortedStrings.diff(allNewCodes_unsortedStrings)

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


  it should "test that all characters can be read and translated" in {

    val sinplifiedCharsAndWords: Set[AcedictEntry] = coll.simplifiedWords ++ coll.simplifiedAllHanItems

    val traditionalCharsAndWords: Set[AcedictEntry]  = coll.traditionalWords ++ coll.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = conwaymap.map(x => x._1.char).toSet

    val notFoundInConway: Set[String] = total
      .filter(x => x.codePoints().count() == 1)
      .filter( xString => !conwayStrings.contains(xString))

    val notFoundInCedict: Set[String] = conwayStrings.filter(x => !total.contains(x))

    notFoundInConway shouldBe Set("ˋ", "ㄏ", "·", "π")//Set("ㄅ", "ˋ", "，", "ㄏ", "：", "·", "π")
    notFoundInCedict.size shouldBe 13719

  }
  
  it should "test that single char can be translated" in {

    val test1 = AgenerateTranslation.getTranslationFromChineseString(
      "誠", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test1 shouldBe ("誠", Set(
      ("ikao", PossibleWordCodes.FirstFirstFirstLastCode),
      ("ikae", PossibleWordCodes.FirstFirstFirstLastCode),
      ("hhxhpo", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode),
      ("hhxhpe", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode),
      ("thxhpo", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode),
      ("thxhpe", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)))

    // hhxhpe  ikao  hhxhpo  thxhpo  ikae  thxhpe

    val test2 = AgenerateTranslation.getTranslationFromChineseString(
      "子", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test2 shouldBe ("子",Set(
      ("fh", TwoCode),
      ("fhzz", PossibleWordCodes.FirstFirstFirstLastCode),
      ("fhzzzz", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)))
    
    //⺒
    val test3 = AgenerateTranslation.getTranslationFromChineseString(
      "⺒", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test3 shouldBe("⺒", Set(
      ("gg", TwoCode),
      ("ggzz", PossibleWordCodes.FirstFirstFirstLastCode),
      ("ggzzzz", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)))

    
    //⻭ and other characters exists as conway and ids codes, but cant be found
    //by the ids map object at runtime. 
    //U+2EED	⻭	212143123452
    //U+2EED	⻭	⿱止⿶凵米
    val test4 = AgenerateTranslation.getTranslationFromChineseString(
      "⻭", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test4 shouldBe("⻭", Set(
      ("nnef", PossibleWordCodes.FirstFirstFirstLastCode),
      ("nnejof", PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)))

  }

  it should "test that a word can be translated" in {

    val test1 = AgenerateTranslation.getTranslationFromChineseString(
      "摳腳", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    test1 shouldBe ("摳腳" , Set(("lmphf", FiveCode), ("lmptf", FiveCode), ("lmp", ThreeCode)))
    
    val test2 = AgenerateTranslation.getTranslationFromChineseString(
      "外東北", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    val conwaytest1 = conwaymap.get(Agrapheme("北"))
    //211(15|35|53)
    test2 shouldBe("外東北", Set(("pjond", FiveCode), ("pjonp", FiveCode), ("pjonm", FiveCode)))

    val testC1 = AgenerateTranslation.getTranslationFromChineseString(
      "箭头", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testC1._2.map(x => x._1).toSet shouldBe Set("fbwkt", "fbw")

    val testD1 = AgenerateTranslation.getTranslationFromChineseString(
      "外東北", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testD1._2.map(x => x._1).toSet shouldBe Set("pjond", "pjonp", "pjonm")

    val testE1 = AgenerateTranslation.getTranslationFromChineseString(
      "木蘭", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testE1._2.map(x => x._1).toSet shouldBe Set("dnjoz", "djjoz", "djboz", "djb", "dnj", "djj")

    val testF1 = AgenerateTranslation.getTranslationFromChineseString(
      "L照", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testF1._2.map(x => x._1).toSet shouldBe Set("zxhwz", "zxh")

    val testG1 = AgenerateTranslation.getTranslationFromChineseString(
      "越獄", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testG1._2.map(x => x._1).toSet shouldBe Set("jepow", "jep","jed", "jepyw","jop", "jedyw","jopyw", "jodyw","jopow", "jedow","jod", "jodow")

    val testH1 = AgenerateTranslation.getTranslationFromChineseString(
      "母子", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testH1._2.map(x => x._1).toSet shouldBe Set("alf", "atfhz", "atf", "alfhz")

    val testI1 = AgenerateTranslation.getTranslationFromChineseString(
      "手足亲情", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testI1._2.map(x => x._1).toSet shouldBe Set("ljtcn", "ljtch", "ljtrn", "ljtwn", "ljtrh", "ljtwh")

    val testJ1 = AgenerateTranslation.getTranslationFromChineseString(
      "連詞", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testJ1._2.map(x => x._1).toSet shouldBe Set("esi", "esigg")

    val testK1 = AgenerateTranslation.getTranslationFromChineseString(
      "血鬱", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testK1._2.map(x => x._1).toSet shouldBe Set("uny", "unyji")

    val testL1 = AgenerateTranslation.getTranslationFromChineseString(
      "手足亲情", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testL1._2.map(x => x._1).toSet shouldBe Set("ljtcn", "ljtch", "ljtrn", "ljtwn", "ljtrh", "ljtwh")

    val testM1 = AgenerateTranslation.getTranslationFromChineseString(
      "連詞", conwaymap, idsmap, idsToStrokeMap, basicTranslation
    )
    testM1._2.map(x => x._1).toSet shouldBe Set("esi", "esigg")
    

  }

}
















