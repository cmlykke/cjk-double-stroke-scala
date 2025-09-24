package AdataSources

import Adatasources.FileReaders.AreadCedictData
import Atypes.{AcedictColl, AcedictEntry, Aconway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Acedicttest extends AnyFlatSpec with Matchers {

  it should "test that comparisons of equal and unequal cedict words are correct" in {
    val test1: AcedictEntry = AcedictEntry("a")
    val test2: AcedictEntry = AcedictEntry("a")
    val test3: AcedictEntry = AcedictEntry("b")

    test1 shouldEqual test2
    test1 should not equal test3

    //潜泳
    val test4: AcedictEntry = AcedictEntry("潜泳")
    val test5: AcedictEntry = AcedictEntry("潜泳")
    test4 shouldEqual test5
  }

  it should "test that cedict work" in {

    val coll: AcedictColl = AreadCedictData.listCedictData()

    coll.simplified.size shouldBe 119038
    coll.traditional.size shouldBe 119038

    val allSimpStrings: Set[String] = coll.simplified.map(x => x.rawEntry).toSet
    val allTradStrings: Set[String] = coll.traditional.map(x => x.rawEntry).toSet

    allSimpStrings.size shouldBe 119038
    allTradStrings.size shouldBe 119038

    allSimpStrings shouldEqual allTradStrings
  }
  /*
  "ConwayNotation" should "throw IllegalArgumentException for empty input list" in {
    intercept[IllegalArgumentException] {  // Use intercept for cleaner assertion.
      new Aconway(List.empty[String])
    }
  }*/
}
