package AcodegenerationTests

import Adatasources.ManualData.AtextType
import Asingletons.AsingletonsForTests
import Atypes.{Agrapheme, AsortingObject, SortingCodes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AtestOutputStatistics extends AnyFlatSpec with Matchers {

  val sorted_sortCharactersSimplified: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedSimp
  val sorted_sortCharactersTraditional: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedTrad


  it should "test sorting two letters" in {

    val simplifiedSingleHits: Set[String] = sorted_sortCharactersSimplified
        .filter(y => y._3.graphemes.length == 1)
        .filter(x =>
          !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    val simplifiedWordHits: Set[String] = sorted_sortCharactersSimplified
      .filter(y => y._3.graphemes.length > 1)
      .filter(x =>
        !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    val traditionalSingleHits: Set[String] = sorted_sortCharactersTraditional
      .filter(y => y._3.graphemes.length == 1)
      .filter(x =>
        !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    val traditionalWordHits: Set[String] = sorted_sortCharactersTraditional
      .filter(y => y._3.graphemes.length > 1)
      .filter(x =>
        !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    simplifiedSingleHits.size shouldBe 28318
    traditionalSingleHits.size shouldBe 28318

    simplifiedWordHits.size shouldBe 179753
    traditionalWordHits.size shouldBe 179753


  }


  it should "test that codes contain elements" in {

    val elementCodes: String = sorted_sortCharactersSimplified
      .filter(y => y._3.sortingCriteria == SortingCodes.OneCodeElem)
      .sortBy(t => (t._2, t._1))
      .map(z => z._1 + z._2).toList.mkString("")

    elementCodes shouldBe
      "木d" +
        "車e" +
        "⺮f竹f" +
        "言i訁i" +
        "⻊j足j𧾷j" +
        "目k⺘" +
        "l手l扌l" +
        "⻝o⻞o⻟o食o飠o" +
        "門p" +
        "金r" +
        "虫s" +
        "⺯u糸u糹u" +
        "馬w"
  }

  it should "test elem codes should come first" in {

    val elementCodes: String = sorted_sortCharactersSimplified
      .filter(y => y._2 == "o")
      .map(z => z._1).toList.mkString("")

    elementCodes shouldBe "食人入八乂〤㐅飠⻝⻞⻟𠆢"
    
  }

  it should "test that no Junda codes under 5000 are outside the first 9, and test a few above it" in {

    val elementCodes: Set[String] =
      sorted_sortCharactersSimplified
      .filter(y => y._2.length == 4)
      .groupBy(_._2)
      .values
      .filter(x => x.length > 9)
      .map(x => x.drop(9))
      .flatten
      .map(x => x._1).toSet

    elementCodes.size shouldBe 2687

    val getNonSimp = elementCodes
      .filter(x => AsingletonsForTests.junda.contains(x))
      .map(x => AsingletonsForTests.junda.get(x).get)
      .toList.sorted

    val failedOrClose = getNonSimp.take(3).toSet
    failedOrClose shouldBe Set(4933, 4915, 3734)

    val getFailed = AsingletonsForTests.junda.filter(x => failedOrClose.contains(x._2)).map(x => x._1).toSet
    getFailed shouldBe Set("類", "藥", "長") // == 类 药 长 // all three "類", "藥", "長" are actually traditional characters


  }


  it should "test that no Tzai codes under 5000 are outside the first 9, and test a few above it" in {

    val elementCodes: Set[String] =
      sorted_sortCharactersTraditional
        .filter(y => y._2.length == 4)
        .groupBy(_._2)
        .values
        .filter(x => x.length > 9)
        .map(x => x.drop(9))
        .flatten
        .map(x => x._1).toSet

    elementCodes.size shouldBe 2698

    val getNonSimp = elementCodes
      .filter(x => AsingletonsForTests.tzai.contains(x))
      .map(x => AsingletonsForTests.tzai.get(x).get)
      .toList.sorted

    val failedOrClose = getNonSimp.take(1).toSet
    failedOrClose shouldBe Set(3572)

    val getFailed = AsingletonsForTests.tzai.filter(x => failedOrClose.contains(x._2)).map(x => x._1).toSet
    getFailed shouldBe Set("葔") // "葔" is common according to Tzai, but is not in Cedict dictionary


  }



  }
