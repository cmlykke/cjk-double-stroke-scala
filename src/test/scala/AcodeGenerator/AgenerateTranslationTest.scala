package AcodeGenerator

import AcodeGenerators.AgenerateTranslation
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Asingletons.AsingletonsForTests
import Atypes.SortingCodes.{FiveCode, FourCode, SixCode, ThreeCodeSingleChar, ThreeCodeTwoCharWord, TwoCode}
import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}
import GenerateOutput.GenerateOutputStrings
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*


class AgenerateTranslationTest extends AnyFlatSpec with Matchers {

  private def getTranslationsFromText(text: String): (String, Set[(String, AsortingCriteria)]) = {
    return AgenerateTranslation.getTranslationFromChineseString(
      text, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation
    )
  }
  
  it should "generateCodesForAllCharacters" in {
    val sinplifiedCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.simplifiedWords ++ AsingletonsForTests.cedict.simplifiedAllHanItems
    val traditionalCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.traditionalWords ++ AsingletonsForTests.cedict.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = AsingletonsForTests.conwaymap.map(x => x._1.char).toSet

    val allChineseStr: Set[String] = total ++ conwayStrings

    val allToSingle: Set[String] = allChineseStr.map(x => wordToSingle(x)).flatten

    val notFoundConway: List[String] = allToSingle.filter(x => !AsingletonsForTests.conwaymap.contains(Agrapheme(x))).toList.sorted
    val notFoundIds: List[String] = allToSingle.filter(x => !AsingletonsForTests.idsmap.contains(Agrapheme(x))).toList.sorted


    //new codes
    val allCodes: Set[(String, Set[(String, AsortingCriteria)])] =
      AgenerateTranslation.translationsOfSetOfStrings(
        allChineseStr, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation)

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

    val sinplifiedCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.simplifiedWords ++ AsingletonsForTests.cedict.simplifiedAllHanItems

    val traditionalCharsAndWords: Set[AcedictEntry]  = AsingletonsForTests.cedict.traditionalWords ++ AsingletonsForTests.cedict.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = AsingletonsForTests.conwaymap.map(x => x._1.char).toSet

    val notFoundInConway: Set[String] = total
      .filter(x => x.codePoints().count() == 1)
      .filter( xString => !conwayStrings.contains(xString))

    val notFoundInCedict: Set[String] = conwayStrings.filter(x => !total.contains(x))

    notFoundInConway shouldBe Set("ˋ", "ㄏ", "·", "π")//Set("ㄅ", "ˋ", "，", "ㄏ", "：", "·", "π")
    notFoundInCedict.size shouldBe 13719

  }
  
  it should "test that single char can be translated" in {

    val test1 = getTranslationsFromText("誠")
    test1 shouldBe ("誠", Set(
      ("ikao", SortingCodes.FourCode),
      ("ikae", SortingCodes.FourCode),
      ("hhxhpo", SortingCodes.SixCode),
      ("hhxhpe", SortingCodes.SixCode),
      ("thxhpo", SortingCodes.SixCode),
      ("thxhpe", SortingCodes.SixCode)))

    // hhxhpe  ikao  hhxhpo  thxhpo  ikae  thxhpe

    val test2 = getTranslationsFromText("子")
    test2 shouldBe ("子",Set(
      ("fh", TwoCode),
      ("fhzz", SortingCodes.FourCode),
      ("fhzzzz", SortingCodes.SixCode)))
    
    //⺒
    val test3 = getTranslationsFromText("⺒")
    test3 shouldBe("⺒", Set(
      ("gg", TwoCode),
      ("ggzz", SortingCodes.FourCode),
      ("ggzzzz", SortingCodes.SixCode)))

    
    //⻭ and other characters exists as conway and ids codes, but cant be found
    //by the ids map object at runtime. 
    //U+2EED	⻭	212143123452
    //U+2EED	⻭	⿱止⿶凵米
    val test4 = getTranslationsFromText("⻭")
    test4 shouldBe("⻭", Set(
      ("nnef", SortingCodes.FourCode),
      ("nnejof", SortingCodes.SixCode)))


    val test5 = getTranslationsFromText("彎")
    test5 shouldBe("彎", Set(
      ("iarm", SortingCodes.FourCode),
      ("iawm", SortingCodes.FourCode),
      ("hhxmsm", SortingCodes.SixCode),
      ("thxmsm", SortingCodes.SixCode)))

    val test5a = getTranslationsFromText("鬱")
    test5a shouldBe("鬱", Set(
      ("yjfi", SortingCodes.FourCode),
      ("yjfjoi", SortingCodes.SixCode)))


    val test6 = getTranslationsFromText("术")
    test6 shouldBe("术", Set(
      ("dtzz", SortingCodes.FourCode),
      ("dt", SortingCodes.TwoCode),
      ("jotzzz", SortingCodes.SixCode)))


    val test7 = getTranslationsFromText("遤")
    test7 shouldBe("遤", Set(
      ("wws", SortingCodes.ThreeCodeSingleChar),
      ("wqt", SortingCodes.ThreeCodeSingleChar),
      ("wqs", SortingCodes.ThreeCodeSingleChar),
      ("wwsz", SortingCodes.FourCode),
      ("wqtz", SortingCodes.FourCode),
      ("wqsz", SortingCodes.FourCode),
      ("nhxwws", SortingCodes.SixCode),
      ("jhxwws", SortingCodes.SixCode),
      ))


    val test8 = getTranslationsFromText("七")
    test8 shouldBe("七", Set(
      ("m", SortingCodes.OneCode),
      ("mzzz", SortingCodes.FourCode),
      ("mzzzzz", SortingCodes.SixCode)))

    val test9 = getTranslationsFromText("虫")
    test9 shouldBe("虫", Set(
      ("s", SortingCodes.OneCode),
      ("szzz", SortingCodes.FourCode),
      ("xjlzzz", SortingCodes.SixCode)))

  }

  it should "test that a word can be translated" in {

    val test1 = getTranslationsFromText("摳腳")
    test1 shouldBe ("摳腳" , Set(("lmphf", FiveCode), ("lmptf", FiveCode), ("lmp", ThreeCodeTwoCharWord)))
    
    val test2 = getTranslationsFromText("外東北")
    val conwaytest1 = AsingletonsForTests.conwaymap.get(Agrapheme("北"))
    //211(15|35|53)
    test2 shouldBe("外東北", Set(("pjond", FiveCode), ("pjonp", FiveCode), ("pjonm", FiveCode)))

    val testC1 = getTranslationsFromText("箭头")
    testC1._2.map(x => x._1).toSet shouldBe Set("fbwkt", "fbw")

    val testD1 = getTranslationsFromText("外東北")
    testD1._2.map(x => x._1).toSet shouldBe Set("pjond", "pjonp", "pjonm")

    val testE1 = getTranslationsFromText("木蘭")
    testE1._2.map(x => x._1).toSet shouldBe Set("dnjoz", "djjoz", "djboz", "djb", "dnj", "djj")

    val testF1 = getTranslationsFromText("L照")
    testF1._2.map(x => x._1).toSet shouldBe Set("zxhwz", "zxh")

    val testG1 = getTranslationsFromText("越獄")
    testG1._2.map(x => x._1).toSet shouldBe Set("jepow", "jep","jed", "jepyw","jop", "jedyw","jopyw", "jodyw","jopow", "jedow","jod", "jodow")

    val testH1 = getTranslationsFromText("母子")
    testH1._2.map(x => x._1).toSet shouldBe Set("alf", "atfhz", "atf", "alfhz")

    val testI1 = getTranslationsFromText("手足亲情")
    testI1._2.map(x => x._1).toSet shouldBe Set("ljtcn", "ljtch", "ljtrn", "ljtwn", "ljtrh", "ljtwh")

    val testJ1 = getTranslationsFromText("連詞")
    testJ1._2.map(x => x._1).toSet shouldBe Set("esi", "esigg")

    val testK1 = getTranslationsFromText("血鬱")
    testK1._2.map(x => x._1).toSet shouldBe Set("uny", "unyji")

    val testL1 = getTranslationsFromText("手足亲情")
    testL1._2.map(x => x._1).toSet shouldBe Set("ljtcn", "ljtch", "ljtrn", "ljtwn", "ljtrh", "ljtwh")

    val testM1 = getTranslationsFromText("連詞")
    testM1._2.map(x => x._1).toSet shouldBe Set("esi", "esigg")
    

  }

}
















