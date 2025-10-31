package AsortingTests

import AcodeGenerators.AgenerateTranslation
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


class AtestSorting extends AnyFlatSpec with Matchers {

  it should "test sortingWorks" in {

    val allCodes: Set[(String, Set[(String, AsortingCriteria)])] =
      AgenerateTranslation.translationsOfSetOfStrings(AsingletonsForTests.chineseTextitems, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation)

    val output: Map[String, Set[(String, String, AsortingCriteria)]] = AsortWordsAndCharacters.convertTranslatedTextToSortFormat(allCodes)

    val sortCharacters: List[(String, String, AsortingObject)] =
      AsortWordsAndCharacters.sortCodes(
        output, 
        AsingletonsForTests.cedict, 
        AsingletonsForTests.junda, 
        AsingletonsForTests.tzai,
        AtextType.Simplified)

    val test = ""
  }

  //priority 1 sorting
  it should "test lettercode" in {
    //lettercode
    val test1a: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abcd")
    val test1b: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abce")

    val testcomp = test1a.compare(test1b)
    testcomp shouldBe -1

    //lettercode
    val test2a: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abcd")
    val test2b: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp2 = test2a.compare(test2b)
    testcomp2 shouldBe 1

  }

  //priority 2 sorting
  it should "test sorting criteria" in {

    //sorting criteria
    val test5a: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test5b: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.TwoCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp5 = test5a.compare(test5b)
    testcomp5 shouldBe -1
  }


  //priority 3 sorting
  it should "test cedict ordering primary" in {

    val test6a: AsortingObject = AsortingObject(
      List(false), List(true), List(-20), List(-20), SortingCodes.OneCode, List("的", "馬", "足", "手"), "abc")
    val test6b: AsortingObject = AsortingObject(
      List(true), List(false), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp6 = test6a.compare(test6b)
    testcomp6 shouldBe 1

    val test7a: AsortingObject = AsortingObject(
      List(true), List(false), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test7b: AsortingObject = AsortingObject(
      List(false), List(true), List(-20), List(-20), SortingCodes.OneCode, List("的", "馬", "足", "手"), "abc")

    val testcomp7 = test7a.compare(test7b)
    testcomp7 shouldBe -1
  }

  //priority 4 sorting
  it should "test ordering charset primary" in {

    val test6a: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test6b: AsortingObject = AsortingObject(
      List(true), List(false), List(-20), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp6 = test6a.compare(test6b)
    testcomp6 shouldBe 1

    val test7a: AsortingObject = AsortingObject(
      List(true), List(false), List(-20), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test7b: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp7 = test7a.compare(test7b)
    testcomp7 shouldBe -1
  }


  //priority 5 sorting
  it should "test ordering charset secondary" in {

    val test6a: AsortingObject = AsortingObject(
      List(true), List(true), List(-20), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test6b: AsortingObject = AsortingObject(
      List(true), List(false), List(-20), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp6 = test6a.compare(test6b)
    testcomp6 shouldBe 1

    val test7a: AsortingObject = AsortingObject(
      List(true), List(false), List(-20), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test7b: AsortingObject = AsortingObject(
      List(true), List(true), List(-20), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp7 = test7a.compare(test7b)
    testcomp7 shouldBe -1
  }


  //priority 6 sorting
  it should "test ordering cedict secondary" in {

    val test6a: AsortingObject = AsortingObject(
      List(true), List(false), List(-20), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test6b: AsortingObject = AsortingObject(
      List(true), List(true), List(-20), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp6 = test6a.compare(test6b)
    testcomp6 shouldBe 1

    val test7a: AsortingObject = AsortingObject(
      List(true), List(true), List(-20), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test7b: AsortingObject = AsortingObject(
      List(true), List(false), List(-20), List(-20), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp7 = test7a.compare(test7b)
    testcomp7 shouldBe -1
  }


  //priority 7 sorting
  it should "test ordering hanchar" in {

    val test6a: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")
    val test6b: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("的", "馬", "足", "手"), "abc")

    val testcomp6 = test6a.compare(test6b)
    testcomp6 shouldBe 1

    val test7a: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("的", "馬", "足", "手"), "abc")
    val test7b: AsortingObject = AsortingObject(
      List(true), List(true), List(-10), List(-10), SortingCodes.OneCode, List("辭", "馬", "足", "手"), "abc")

    val testcomp7 = test7a.compare(test7b)
    testcomp7 shouldBe -1
  }


}
