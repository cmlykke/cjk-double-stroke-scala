package AdataSources

import Adatasources.FileReaders.AjundaData
import Asingletons.AsingletonsForTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.*

import scala.collection.immutable

class AjundaDataTest extends AnyFlatSpec with Matchers {

  val tolerance = 0.000001

  it should "test that junda works" in {
    val test = ""

    val test1 = AsingletonsForTests.junda.get("的").get
    test1 shouldBe 1

    val test2 = AsingletonsForTests.junda.get("一").get
    test2 shouldBe 2

    val test3 = AsingletonsForTests.junda.get("是").get
    test3 shouldBe 3

    val test4 = AsingletonsForTests.junda.get("鱓").get
    test4 shouldBe 9931

    val test5 = AsingletonsForTests.junda.get("鲖").get
    test5 shouldBe 9932

    val test6 = AsingletonsForTests.junda.get("鴒").get
    test6 shouldBe 9933

    val test10 = ""
  }
}
