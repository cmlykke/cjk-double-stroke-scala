package AgraphemeToCodeConverters

import Adatasources.FileReaders.{AidsData, AreadConwayData}
import Adatasources.ManualData.Aelements
import Asingletons.AsingletonsForTests
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AidsRecur}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable
import scala.collection.immutable.HashMap

class Aidsrecurtest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private def testElementMatch(input: String,
                               expected: List[String]): Unit = {
    val originalConway: Option[AconwayColl] = AsingletonsForTests.conwaymap.get(Agrapheme(input))
    if (originalConway.isEmpty) {
      throw new RuntimeException("AidsRecur conway not found")
    }
    val recurtree = AidsRecur(Agrapheme(input), AsingletonsForTests.idsmap, AsingletonsForTests.conwaymap, originalConway.get.rawConway.rawConway)

    val conwayRaw: Option[AconwayColl] = AsingletonsForTests.conwaymap.get(Agrapheme(input))
    val backslashCleanedList: List[String] = conwayRaw.get.rawConway.rawConway
      .map(x => AgraphemeToStrokeSet.unrollBackSlash(x))

    val elemtree: List[Option[String]] = backslashCleanedList.map(x => AidsRecur.findElementmatchHelper(Agrapheme(input), recurtree, AsingletonsForTests.idsToStrokeMap, x))

    val theyareallthere: Set[String] = elemtree.filter(x => x.isDefined).map(x => x.get).toSet //.collect { case Some(s) => s }
    theyareallthere.toList shouldBe expected
  }

  it should "should test that 言 gets found in 彎" in {
    val test = ""
    val originalConway: Option[AconwayColl] = AsingletonsForTests.conwaymap.get(Agrapheme("彎"))
    if (originalConway.isEmpty) {
      throw new RuntimeException("AidsRecur conway not found")
    }
    val recurtree = AidsRecur(Agrapheme("彎"),AsingletonsForTests.idsmap, AsingletonsForTests.conwaymap, originalConway.get.rawConway.rawConway)

    val conwayRaw: Option[AconwayColl] = AsingletonsForTests.conwaymap.get(Agrapheme("彎"))
    val backslashCleanedList: List[String] = conwayRaw.get.rawConway.rawConway
      .map(x => AgraphemeToStrokeSet.unrollBackSlash(x))

    val elemtree: List[Option[String]] = backslashCleanedList
      .map(x => AidsRecur.findElementmatchHelper(Agrapheme("彎"), recurtree, AsingletonsForTests.idsToStrokeMap, x))

    val theyareallthere: Set[String] = elemtree.filter(x => x.isDefined).map(x => x.get).toSet //.collect { case Some(s) => s }
    theyareallthere.toList shouldBe List("言")
  }

  it should "test that 木 is found as an element in 本" in {
    //木
    testElementMatch("术", List("木"))
    testElementMatch("本", List("木"))

  }

  it should "test that elements that doesnt follow strokeorder gets moved" in {
    //辶
    testElementMatch("遤", List("馬"))
  }

  it should "test that idsrecur finds the correct first elements" in {

    //"七" // no element
    testElementMatch("七", List())
    
    //虫
    testElementMatch("虫", List("虫"))
    testElementMatch("蛜", List("虫"))
    testElementMatch("浊", List())

    //木
    testElementMatch("木", List("木"))
    testElementMatch("枝", List("木"))
    testElementMatch("床", List())
    testElementMatch("鬱", List())

    //竹
    testElementMatch("竹", List("竹"))
    testElementMatch("箴", List("竹"))
    testElementMatch("癤", List())

    //足
    testElementMatch("足", List("足"))
    testElementMatch("跟", List("⿱口止"))
    testElementMatch("跫", List())

    //目
    testElementMatch("竹", List("竹"))
    testElementMatch("箴", List("竹"))
    testElementMatch("癤", List())

    //手
    testElementMatch("手", List("手"))
    testElementMatch("扎", List("扌"))
    testElementMatch("摰", List())

    //馬
    testElementMatch("馬", List("馬"))
    testElementMatch("馸", List("馬"))
    testElementMatch("傌", List())

    //車
    testElementMatch("車", List("車"))
    testElementMatch("斬", List("車"))
    testElementMatch("輝", List())

    //金
    testElementMatch("金", List("金"))
    testElementMatch("銪", List("金"))
    testElementMatch("銮", List())

    //糸
    testElementMatch("糸", List("糸"))
    testElementMatch("結", List("糹"))
    testElementMatch("乿", List())

    //言
    testElementMatch("言", List("言"))
    testElementMatch("訪", List("言"))
    testElementMatch("霅", List())
    testElementMatch("彎", List("言"))

    //食
    testElementMatch("食", List("食"))
    testElementMatch("餞", List("飠"))
    testElementMatch("飡", List())

    //門
    testElementMatch("門", List("門"))
    testElementMatch("閾", List("門"))
    testElementMatch("㥃", List())

  }


}