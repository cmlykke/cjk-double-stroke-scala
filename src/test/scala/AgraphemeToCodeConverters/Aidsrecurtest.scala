package AgraphemeToCodeConverters

import Adatasources.FileReaders.{AidsData, AreadConwayData}
import Adatasources.ManualData.Aelements
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AidsRecur}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable
import scala.collection.immutable.HashMap

class Aidsrecurtest extends AnyFlatSpec with Matchers {

  val idsdata: HashMap[Agrapheme, String]  = AidsData.idsDataRaw()
  val idsToStrokeMap:  Map[String, Aelementstype] = Aelements.idsToStrokeMap
  val conwaymap: immutable.HashMap[Agrapheme, AconwayColl] = AreadConwayData.mapConwayData()

    //immutable.HashMap[Agrapheme, AconwayColl]
    //mapConwayData()
    //AreadConwayData
    
  def testElementMatch(input: String, expected: List[String], idsdataInput: HashMap[Agrapheme, String]): Unit = {
    val recurtree = AidsRecur(Agrapheme(input), idsdataInput)
    
    val conwayRaw: Option[AconwayColl] = conwaymap.get(Agrapheme(input))
    val backslashCleanedList: List[String] = conwayRaw.get.rawConway.rawConway
      .map(x => AgraphemeToStrokeSet.unrollBackSlash(x))
    
    val elemtree: List[Option[String]] = backslashCleanedList.map(x => AidsRecur.findElementmatchHelper(Agrapheme(input), recurtree, idsToStrokeMap,x))
    
    val theyareallthere: Set[String] = elemtree.filter(x => x.isDefined).map(x => x.get).toSet//.collect { case Some(s) => s }
    theyareallthere.toList shouldBe expected
  }

  it should "should test that 言 gets found in 彎" in {
    testElementMatch("彎", List("言"), idsdata)
  }

  it should "test that 木 is found as an element in 本" in {
    //木
    testElementMatch("术", List("木"), idsdata)
    testElementMatch("本", List("木"), idsdata)

  }

  it should "test that elements that doesnt follow strokeorder gets moved" in {
    //辶
    testElementMatch("遤", List("馬"), idsdata)
  }

  it should "test that idsrecur finds the correct first elements" in {

    //"七" // no element
    testElementMatch("七", List(), idsdata)
    
    //虫
    testElementMatch("虫", List("虫"), idsdata)
    testElementMatch("蛜", List("虫"), idsdata)
    testElementMatch("浊", List(), idsdata)

    //木
    testElementMatch("木", List("木"), idsdata)
    testElementMatch("枝", List("木"), idsdata)
    testElementMatch("床", List(), idsdata)

    //竹
    testElementMatch("竹", List("竹"), idsdata)
    testElementMatch("箴", List("竹"), idsdata)
    testElementMatch("癤", List(), idsdata)

    //足
    testElementMatch("足", List("足"), idsdata)
    testElementMatch("跟", List("⿱口止"), idsdata)
    testElementMatch("跫", List(), idsdata)

    //目
    testElementMatch("竹", List("竹"), idsdata)
    testElementMatch("箴", List("竹"), idsdata)
    testElementMatch("癤", List(), idsdata)

    //手
    testElementMatch("手", List("手"), idsdata)
    testElementMatch("扎", List("扌"), idsdata)
    testElementMatch("摰", List(), idsdata)

    //馬
    testElementMatch("馬", List("馬"), idsdata)
    testElementMatch("馸", List("馬"), idsdata)
    testElementMatch("傌", List(), idsdata)

    //車
    testElementMatch("車", List("車"), idsdata)
    testElementMatch("斬", List("車"), idsdata)
    testElementMatch("輝", List(), idsdata)

    //金
    testElementMatch("金", List("金"), idsdata)
    testElementMatch("銪", List("金"), idsdata)
    testElementMatch("銮", List(), idsdata)

    //糸
    testElementMatch("糸", List("糸"), idsdata)
    testElementMatch("結", List("糹"), idsdata)
    testElementMatch("乿", List(), idsdata)

    //言
    testElementMatch("言", List("言"), idsdata)
    testElementMatch("訪", List("言"), idsdata)
    testElementMatch("霅", List(), idsdata)

    //食
    testElementMatch("食", List("食"), idsdata)
    testElementMatch("餞", List("飠"), idsdata)
    testElementMatch("飡", List(), idsdata)

    //門
    testElementMatch("門", List("門"), idsdata)
    testElementMatch("閾", List("門"), idsdata)
    testElementMatch("㥃", List(), idsdata)

  }


}