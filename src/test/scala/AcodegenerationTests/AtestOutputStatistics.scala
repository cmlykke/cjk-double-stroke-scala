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
    getFailed shouldBe Set("葔")
    // "葔" is common according to Tzai, but is not in Cedict dictionary
    // used only in the word: 薃葔

  }


  it should "test get a string with Junda codes up to 6000" in {

    val elementCodes: String =
      sorted_sortCharactersSimplified
        .filter(y => y._2.length == 4)
        .groupBy(_._2)
        .values
        .filter(x => x.length > 9)
        .map(x => x.drop(9))
        .map(x => x.filter(y => AsingletonsForTests.junda.contains(y._1)))
        .map(x => x.map(y => (y._1, y._2, AsingletonsForTests.junda.get(y._1).get)))
        .map(x => x.filter(y => y._3 <= 6000))
        .filter( x => x.size > 0)
        .toList.sortBy(_.head._3)
        .map(x => (x.head._2, x))
        .map(x => (x._1, x._2.map(y => (y._1 + y._3).mkString("")).mkString(" ")))
        .map(x => x._1 + " " + x._2).mkString("")

    elementCodes shouldBe
      "jjuo 藥3734" +
        "njuo 藥3734" +
        "ejoo 類4915" +
        "jhmo 長4933" +
        "nhmo 長4933" +
        "jnho 聩5209 綦5599" +
        "xhjo 嗉5351 題5105" +
        "kxho 砹5772 硖5856" +
        "yhso 锿5941" +
        "pxjg 鮈5959" +
        "jnxn 難5970"


  }
  
  it should "test get a string with Tzai codes up to 6000" in {

    val elementCodes: String =
      sorted_sortCharactersTraditional
        .filter(y => y._2.length == 4)
        .groupBy(_._2)
        .values
        .filter(x => x.length > 9)
        .map(x => x.drop(9))
        .map(x => x.filter(y => AsingletonsForTests.tzai.contains(y._1)))
        .map(x => x.map(y => (y._1, y._2, AsingletonsForTests.tzai.get(y._1).get)))
        .map(x => x.filter(y => y._3 <= 6000))
        .filter( x => x.size > 0)
        .toList.sortBy(_.head._3)
        .map(x => (x.head._2, x))
        .map(x => (x._1, x._2.map(y => (y._1 + y._3).mkString("")).mkString(" ")))
        .map(x => x._1 + " " + x._2).mkString("")

    elementCodes shouldBe
      "jjuo 葔3572" +
        "njuo 葔3572" +
        "yveo 秝5100" +
        "hnjo 瑮5135" +
        "jnxo 鞅5412 顴5454 茦5879" +
        "kxho 饜5477 戛5769" +
        "pxjh 魽5613" +
        "njjh 荺5624" +
        "jjjh 荺5624" +
        "jjjn 蓳5631" +
        "njjn 蓳5631" +
        "pxjn 鰡5650" +
        "jjgo 芵5661" +
        "xjgo 嘳5724" +
        "xhjo 暪5811" +
        "wjgo 漯5821"


  }

  }
