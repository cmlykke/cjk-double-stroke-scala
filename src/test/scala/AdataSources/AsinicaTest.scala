package AdataSources

import Adatasources.FileReaders.AsinicaData
import Asingletons.AsingletonsForTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable

class AsinicaTest extends AnyFlatSpec with Matchers {


  it should "test Taiwan data data" in {

    val sinicaData: immutable.HashMap[String, Int] = AsinicaData.sinicaMap

    sinicaData.size shouldBe 8272

    val test9056 = sinicaData.filter(x => x._2 == 8272).toList
    test9056 shouldBe List(("信息", 8272))

  }
}