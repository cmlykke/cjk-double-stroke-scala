package AdataSources

import Adatasources.FileReaders.AreadConwayData
import Atypes.{Aconway, AconwayColl, Agrapheme}
import UtilityClasses.{ConwayColl, Grapheme, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.HashMap


class Aconwaytest extends AnyFlatSpec with Matchers {  // Renamed to match class; "Spec" suffix is common.

  private val allowedChars: Set[Char] = Set('(', ')', '|', '1', '2', '3', '4', '5', '\\')  // Duplicated for test isolation.

  "ConwayNotation" should "throw IllegalArgumentException for empty input list" in {
    intercept[IllegalArgumentException] {  // Use intercept for cleaner assertion.
      new Aconway(List.empty[String])
    }
  }

  it should "throw IllegalArgumentException for input list with multiple items" in {
    intercept[IllegalArgumentException] {
      new Aconway(List("valid", "extra"))
    }
  }

  it should "throw IllegalArgumentException for empty or whitespace-only string" in {
    intercept[IllegalArgumentException] {
      new Aconway(List(""))
    }
    intercept[IllegalArgumentException] {
      new Aconway(List("   "))
    }
  }

  it should "throw IllegalArgumentException for string with invalid characters" in {
    intercept[IllegalArgumentException] {
      new Aconway(List("a1b2"))  // Mixed valid/invalid.
    }
    intercept[IllegalArgumentException] {
      new Aconway(List("!@#"))  // All invalid.
    }
  }

  it should "create rawConway correctly for valid single-string input" in {
    val input = List("12|(3\\45)")
    val notation = new Aconway(input)
    notation.rawConway shouldBe "12|(3\\45)"  // Test with all allowed chars.
  }

  it should "validate real Conway data from reader" in {
    val data: HashMap[Agrapheme, AconwayColl] = AreadConwayData.mapConwayData()

    val invalidEntries = data.filter { case (_, coll) =>
      coll.rawConway.rawConway.exists(c => !allowedChars.contains(c))  // Assuming AconwayColl has rawConway: ConwayNotation.
    }

    invalidEntries.size shouldBe 0

    data.size shouldBe 28301
  }
}