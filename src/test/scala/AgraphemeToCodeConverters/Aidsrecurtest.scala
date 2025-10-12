package AgraphemeToCodeConverters

import Adatasources.FileReaders.AidsData
import Atypes.{Aelements, Aelementstype, Agrapheme, AidsRecur}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.HashMap

class Aidsrecurtest extends AnyFlatSpec with Matchers {

  val idsdata: HashMap[Agrapheme, String]  = AidsData.idsDataRaw()
  val idsToStrokeMap:  Map[String, Aelementstype] = Aelements.idsToStrokeMap

  def testElementMatch(input: String, expected: Option[String], idsdataInput: HashMap[Agrapheme, String]): Unit = {
    val recurtree = AidsRecur(Agrapheme(input), idsdataInput)
    val elemtree = AidsRecur.findElementmatchHelper(recurtree, idsToStrokeMap)
    elemtree shouldBe expected
    if (expected.isDefined) {
      idsToStrokeMap.contains(expected.get) shouldBe true
    }
  }

  it should "should test that 言 gets found in 彎" in {
    testElementMatch("彎", Option("言"), idsdata)
  }

  it should "test that 木 is found as an element in 本" in {
    //木
    testElementMatch("术", Option("木"), idsdata)
    testElementMatch("本", Option("木"), idsdata)

  }

  it should "test that elements that doesnt follow strokeorder gets moved" in {
    //辶
    testElementMatch("遤", Option("馬"), idsdata)
  }

  it should "test that idsrecur finds the correct first elements" in {
    // test non element
    val recur1: AidsRecur = AidsRecur(Agrapheme("七"), idsdata)
    val elem1: Option[String] = AidsRecur.findElementmatchHelper(recur1, idsToStrokeMap)
    elem1 shouldBe None

    //虫
    testElementMatch("虫", Option("虫"), idsdata)
    testElementMatch("蛜", Option("虫"), idsdata)
    testElementMatch("浊", None, idsdata)

    //木
    testElementMatch("木", Option("木"), idsdata)
    testElementMatch("枝", Option("木"), idsdata)
    testElementMatch("床", None, idsdata)

    //竹
    testElementMatch("竹", Option("竹"), idsdata)
    testElementMatch("箴", Option("竹"), idsdata)
    testElementMatch("癤", None, idsdata)

    //足
    testElementMatch("足", Option("足"), idsdata)
    testElementMatch("跟", Option("⿱口止"), idsdata)
    testElementMatch("跫", None, idsdata)

    //目
    testElementMatch("竹", Option("竹"), idsdata)
    testElementMatch("箴", Option("竹"), idsdata)
    testElementMatch("癤", None, idsdata)

    //手
    testElementMatch("手", Option("手"), idsdata)
    testElementMatch("扎", Option("扌"), idsdata)
    testElementMatch("摰", None, idsdata)

    //馬
    testElementMatch("馬", Option("馬"), idsdata)
    testElementMatch("馸", Option("馬"), idsdata)
    testElementMatch("傌", None, idsdata)

    //車
    testElementMatch("車", Option("車"), idsdata)
    testElementMatch("斬", Option("車"), idsdata)
    testElementMatch("輝", None, idsdata)

    //金
    testElementMatch("金", Option("金"), idsdata)
    testElementMatch("銪", Option("金"), idsdata)
    testElementMatch("銮", None, idsdata)

    //糸
    testElementMatch("糸", Option("糸"), idsdata)
    testElementMatch("結", Option("糹"), idsdata)
    testElementMatch("乿", None, idsdata)

    //言
    testElementMatch("言", Option("言"), idsdata)
    testElementMatch("訪", Option("言"), idsdata)
    testElementMatch("霅", None, idsdata)

    //食
    testElementMatch("食", Option("食"), idsdata)
    testElementMatch("餞", Option("飠"), idsdata)
    testElementMatch("飡", None, idsdata)

    //門
    testElementMatch("門", Option("門"), idsdata)
    testElementMatch("閾", Option("門"), idsdata)
    testElementMatch("㥃", None, idsdata)

  }


}