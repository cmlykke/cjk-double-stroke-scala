package AdataSources

import Adatasources.FileReaders.{AjundaData, AtzaiData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable

class AtzaiDataTest extends AnyFlatSpec with Matchers {

  val tzai: immutable.HashMap[String, Int] = AtzaiData.generateMapTzaiData()
  val tolerance = 0.000001

  it should "test that junda works" in {
    val test = ""

    val test1 = tzai.get("的").get
    test1 shouldBe 13060

    val test2 = tzai.get("是").get
    test2 shouldBe 13059

    val test3 = tzai.get("不").get
    test3 shouldBe 13058

    val test4 = tzai.get("顁").get
    test4 shouldBe 3

    val test5 = tzai.get("鵧").get
    test5 shouldBe 2

    val test6 = tzai.get("鷍").get
    test6 shouldBe 1

    val test10 = ""
  }
}
