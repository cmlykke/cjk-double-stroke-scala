package AdataSources

import Adatasources.FileReaders.AblcuData
import Asingletons.AsingletonsForTests
import Atypes.AcedictEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable

class AblcuTest extends AnyFlatSpec with Matchers {


  it should "test blcu data" in {

    val blcuData: immutable.HashMap[String, Int] = AsingletonsForTests.blcuData

    blcuData.size shouldBe 1048570

    val test9056 = blcuData.filter(x => x._2 == 9056).toList
    test9056 shouldBe List(("缺失", 9056))
    
  }
}
