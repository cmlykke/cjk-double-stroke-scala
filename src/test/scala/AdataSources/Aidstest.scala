package AdataSources

import Adatasources.FileReaders.AidsData
import Atypes.{Agrapheme, AidsRecur}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.HashMap

class Aidstest extends AnyFlatSpec with Matchers {

  it should "test that idsData Hashmap works" in {
    val idsData: HashMap[Agrapheme, AidsRecur] = AidsData.idsData()

    val test1: AidsRecur = idsData(Agrapheme("㸂"))

    test1.grapheme.char shouldBe "㸂"

    idsData.size shouldBe 89086
  }

}
