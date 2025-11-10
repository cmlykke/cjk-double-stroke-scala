package AdataSources

import Adatasources.FileReaders.{AreadCedictData, AreadConwayData}
import Asingletons.AsingletonsForTests
import Atypes.{AcedictColl, AcedictEntry, Aconway, Agrapheme}
import UtilityClasses.Grapheme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Acedicttest extends AnyFlatSpec with Matchers {

  it should "test that AcedictEntry equality checking works" in {
    val test1: AcedictEntry = AcedictEntry("a")
    val test2: AcedictEntry = AcedictEntry("a")
    val test3: AcedictEntry = AcedictEntry("b")

    test1 shouldEqual test2
    test1 should not equal test3

    val test4: AcedictEntry = AcedictEntry("潜泳")
    val test5: AcedictEntry = AcedictEntry("潜泳")
    test4 shouldEqual test5
  }

  it should "test that cedict contains the expected number of items, " +
    "and only few and irrelevant characters are not found in conway" in {

    val birdInInMap: Boolean = AsingletonsForTests.conwaymap.contains(Agrapheme("䳭"))

    AsingletonsForTests.cedict.simplifiedWords.size shouldBe 119024
    AsingletonsForTests.cedict.traditionalWords.size shouldBe 120172

    val allSimpStrings: Set[String] = AsingletonsForTests.cedict.simplifiedWords.map(x => x.rawEntry).toSet
    val allTradStrings: Set[String] = AsingletonsForTests.cedict.traditionalWords.map(x => x.rawEntry).toSet

    allSimpStrings.size shouldBe 119024
    allTradStrings.size shouldBe 120172

    AsingletonsForTests.cedict.simplifiedAllHanItems.size shouldBe 11009
    AsingletonsForTests.cedict.traditionalAllHanItems.size shouldBe 11923

    val firstTen: String = AsingletonsForTests.cedict.simplifiedAllHanItems
      .map(entry => entry.rawEntry.codePointAt(0))
      .toList.sorted.take(10)
      .map(codepoint => codepoint + "_" + Character.toString(codepoint))
      .mkString(" ")

    val conwaySet: Set[String] = AreadConwayData.conwaySetFunc()
    val birsIsInSet: Boolean = conwaySet.contains("䳭")
    val nonConwa: String = AsingletonsForTests.cedict.allHanCharacters
      .filter(entry => !conwaySet.contains(entry.rawEntry))
      .map(entry => entry.rawEntry.codePointAt(0)).toList.sorted
      .map(codepoint => codepoint + "_" + Character.toString(codepoint))
      .drop(22)
      .take(10)
      .mkString(" ")


    val nonConwaTest: String = AsingletonsForTests.cedict.allHanCharacters
      .filter(entry => !conwaySet.contains(entry.rawEntry))
      .map(entry => entry.rawEntry.codePointAt(0)).toList.sorted
      .map(codepoint => codepoint + "_" + Character.toString(codepoint))
      .mkString("_")

    //many cedict characters are not to be found in conway
    nonConwaTest shouldBe "183_·_715_ˋ_960_π_12289_、_12559_ㄏ_65292_，_65306_："


  }
}
