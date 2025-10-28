package AgraphemeToCodeConverters

import Adatasources.FileReaders.{AidsData, AreadConwayData}
import Adatasources.ManualData.Aelements
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AidsRecur}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable
import scala.collection.immutable.HashMap

class Aidsrecurtest extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  // Defer heavy IO to test execution time
  private lazy val idsdata = Adatasources.FileReaders.AidsData.idsDataRaw()
  private lazy val idsToStrokeMap = Adatasources.ManualData.Aelements.idsToStrokeMap
  private lazy val conwaymap = Adatasources.FileReaders.AreadConwayData.mapConwayData()

  override protected def beforeAll(): Unit = {
    // Touch the lazy vals to fail early and predictably, and to allow breakpoints here
    super.beforeAll()
    val _ = idsdata
    val _2 = idsToStrokeMap
    val _3 = conwaymap
  }

  private def testElementMatch(input: String,
                               expected: List[String],
                               idsdataInput: HashMap[Agrapheme, String],
                               conwaymap: HashMap[Agrapheme, AconwayColl]): Unit = {
    val originalConway: Option[AconwayColl] = conwaymap.get(Agrapheme(input))
    if (originalConway.isEmpty) {
      throw new RuntimeException("AidsRecur conway not found")
    }
    val recurtree = AidsRecur(Agrapheme(input), idsdataInput, conwaymap, originalConway.get.rawConway.rawConway)

    val conwayRaw: Option[AconwayColl] = conwaymap.get(Agrapheme(input))
    val backslashCleanedList: List[String] = conwayRaw.get.rawConway.rawConway
      .map(x => AgraphemeToStrokeSet.unrollBackSlash(x))

    val elemtree: List[Option[String]] = backslashCleanedList.map(x => AidsRecur.findElementmatchHelper(Agrapheme(input), recurtree, idsToStrokeMap, x))

    val theyareallthere: Set[String] = elemtree.filter(x => x.isDefined).map(x => x.get).toSet //.collect { case Some(s) => s }
    theyareallthere.toList shouldBe expected
  }

  it should "should test that 言 gets found in 彎" in {
    val test = ""
    val originalConway: Option[AconwayColl] = conwaymap.get(Agrapheme("彎"))
    if (originalConway.isEmpty) {
      throw new RuntimeException("AidsRecur conway not found")
    }
    val recurtree = AidsRecur(Agrapheme("彎"),idsdata, conwaymap, originalConway.get.rawConway.rawConway)

    val conwayRaw: Option[AconwayColl] = conwaymap.get(Agrapheme("彎"))
    val backslashCleanedList: List[String] = conwayRaw.get.rawConway.rawConway
      .map(x => AgraphemeToStrokeSet.unrollBackSlash(x))

    val elemtree: List[Option[String]] = backslashCleanedList
      .map(x => AidsRecur.findElementmatchHelper(Agrapheme("彎"), recurtree, idsToStrokeMap, x))

    val theyareallthere: Set[String] = elemtree.filter(x => x.isDefined).map(x => x.get).toSet //.collect { case Some(s) => s }
    theyareallthere.toList shouldBe List("言")
  }

  it should "test that 木 is found as an element in 本" in {
    //木
    testElementMatch("术", List("木"), idsdata, conwaymap)
    testElementMatch("本", List("木"), idsdata, conwaymap)

  }

  it should "test that elements that doesnt follow strokeorder gets moved" in {
    //辶
    testElementMatch("遤", List("馬"), idsdata, conwaymap)
  }

  it should "test that idsrecur finds the correct first elements" in {

    //"七" // no element
    testElementMatch("七", List(), idsdata, conwaymap)
    
    //虫
    testElementMatch("虫", List("虫"), idsdata, conwaymap)
    testElementMatch("蛜", List("虫"), idsdata, conwaymap)
    testElementMatch("浊", List(), idsdata, conwaymap)

    //木
    testElementMatch("木", List("木"), idsdata, conwaymap)
    testElementMatch("枝", List("木"), idsdata, conwaymap)
    testElementMatch("床", List(), idsdata, conwaymap)
    testElementMatch("鬱", List(), idsdata, conwaymap)

    //竹
    testElementMatch("竹", List("竹"), idsdata, conwaymap)
    testElementMatch("箴", List("竹"), idsdata, conwaymap)
    testElementMatch("癤", List(), idsdata, conwaymap)

    //足
    testElementMatch("足", List("足"), idsdata, conwaymap)
    testElementMatch("跟", List("⿱口止"), idsdata, conwaymap)
    testElementMatch("跫", List(), idsdata, conwaymap)

    //目
    testElementMatch("竹", List("竹"), idsdata, conwaymap)
    testElementMatch("箴", List("竹"), idsdata, conwaymap)
    testElementMatch("癤", List(), idsdata, conwaymap)

    //手
    testElementMatch("手", List("手"), idsdata, conwaymap)
    testElementMatch("扎", List("扌"), idsdata, conwaymap)
    testElementMatch("摰", List(), idsdata, conwaymap)

    //馬
    testElementMatch("馬", List("馬"), idsdata, conwaymap)
    testElementMatch("馸", List("馬"), idsdata, conwaymap)
    testElementMatch("傌", List(), idsdata, conwaymap)

    //車
    testElementMatch("車", List("車"), idsdata, conwaymap)
    testElementMatch("斬", List("車"), idsdata, conwaymap)
    testElementMatch("輝", List(), idsdata, conwaymap)

    //金
    testElementMatch("金", List("金"), idsdata, conwaymap)
    testElementMatch("銪", List("金"), idsdata, conwaymap)
    testElementMatch("銮", List(), idsdata, conwaymap)

    //糸
    testElementMatch("糸", List("糸"), idsdata, conwaymap)
    testElementMatch("結", List("糹"), idsdata, conwaymap)
    testElementMatch("乿", List(), idsdata, conwaymap)

    //言
    testElementMatch("言", List("言"), idsdata, conwaymap)
    testElementMatch("訪", List("言"), idsdata, conwaymap)
    testElementMatch("霅", List(), idsdata, conwaymap)
    testElementMatch("彎", List("言"), idsdata, conwaymap)

    //食
    testElementMatch("食", List("食"), idsdata, conwaymap)
    testElementMatch("餞", List("飠"), idsdata, conwaymap)
    testElementMatch("飡", List(), idsdata, conwaymap)

    //門
    testElementMatch("門", List("門"), idsdata, conwaymap)
    testElementMatch("閾", List("門"), idsdata, conwaymap)
    testElementMatch("㥃", List(), idsdata, conwaymap)

  }


}