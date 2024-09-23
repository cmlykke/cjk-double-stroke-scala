package OutputTranslation

import UtilityClasses.{CedictEntry, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable

class OutputSortingTest extends AnyFlatSpec with Matchers {


  it should "check the codes of cedict and conwaySansCedict" in {
    val conwaySansCedict: Set[OutputEntry] = OutputSorting.conwayOutSansCedict
    val cedictOut: Set[OutputEntry] = OutputSorting.cedictSetOut

    var overlap: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    var desiredChar: Option[OutputEntry] = None
    for (entry <- conwaySansCedict) {
      if (cedictOut.contains(entry)) {
        overlap.add(entry)
      }
      if (entry.chineseStr == "臒") {
        desiredChar = Some(entry)
      }
    }
    overlap.size shouldBe 0

    desiredChar should not be None
    
    desiredChar.get.codes shouldBe Set("ptfful", "pgfxul", "ptxful", "pgfful", "pgfl", "pgxl",
      "pgxful", "ptfvbl", "ptfl", "ptxl", "pgfvbl", "ptfxul")

    val test: String = ""
  }

}
