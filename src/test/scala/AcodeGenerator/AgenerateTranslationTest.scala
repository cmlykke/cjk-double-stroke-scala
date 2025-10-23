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

    //tests for old and new:

    val a1 = oldCodes_unsortedStrings.filter(x => x._1 == "箭头").map(x => x._2).toSet
    val a2 =  allNewCodes_unsortedStrings.filter(x => x._1 == "箭头").map(x => x._2).toSet
    val a1doublecheck = oldCodes.get("fbw")
    a1 shouldBe a2

    val b1 = oldCodes_unsortedStrings.filter(x => x._1 == "外東北").map(x => x._2).toSet
    val b2 = allNewCodes_unsortedStrings.filter(x => x._1 == "外東北").map(x => x._2).toSet
    b1 shouldBe b2

    val c1 = oldCodes_unsortedStrings.filter(x => x._1 == "木蘭").map(x => x._2).toSet
    val c2 = allNewCodes_unsortedStrings.filter(x => x._1 == "木蘭").map(x => x._2).toSet
    c1 shouldBe c2
    //old: dnjoz djjoz djboz djb dnj djj
    //new: dzjjo dznjo dzjbo dzn dzj

    //L照
    val d1 = oldCodes_unsortedStrings.filter(x => x._1 == "L照").map(x => x._2).toSet
    val d2 = allNewCodes_unsortedStrings.filter(x => x._1 == "L照").map(x => x._2).toSet
    d1 shouldBe d2

    /*
    //new is correct
    val e1 = oldCodes_unsortedStrings.filter(x => x._1 == "手足亲情").map(x => x._2).toSet
    val e2 = allNewCodes_unsortedStrings.filter(x => x._1 == "手足亲情").map(x => x._2).toSet
    val e3 =  allNewCodes_unsortedStrings.filter(x => x._1 == "情").map(x => x._2).toSet
    e1 shouldBe e2

    val f1 = oldCodes_unsortedStrings.filter(x => x._1 == "連詞").map(x => x._2).toSet
    val f2 = allNewCodes_unsortedStrings.filter(x => x._1 == "連詞").map(x => x._2).toSet
    f1 shouldBe f2
        
    val h1 = oldCodes_unsortedStrings.filter(x => x._1 == "血鬱").map(x => x._2).toSet
    val h2 = allNewCodes_unsortedStrings.filter(x => x._1 == "血鬱").map(x => x._2).toSet
    h1 shouldBe h2
*/
    
    //越獄
    val g1 = oldCodes_unsortedStrings.filter(x => x._1 == "越獄").map(x => x._2).toSet
    val g2 = allNewCodes_unsortedStrings.filter(x => x._1 == "越獄").map(x => x._2).toSet
    g1 shouldBe g2


    // 母子  alfhz
    val h1 = oldCodes_unsortedStrings.filter(x => x._1 == "母子").map(x => x._2).toSet
    val h2 = allNewCodes_unsortedStrings.filter(x => x._1 == "母子").map(x => x._2).toSet
    val testh1 = Set("alf", "atfhz", "atf", "alfhz")
    val testh2 = Set("alf", "atfhz", "atf", "alfhz")//Set("alf", "alfhz", "anfhz", "ajf", "anf", "ajfhz")
    h1 shouldBe testh1
    h2 shouldBe testh2

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

  }

}
















