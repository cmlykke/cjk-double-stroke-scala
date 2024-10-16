package staticFileGenerators.HeisigMap

import OutputTranslation.{OutputOverlapObjects, OutputSorting}
import UtilityClasses.{ConwayColl, Grapheme, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.{GenerateConwayCodes, ReadConwayData}

import scala.collection.{SortedMap, mutable}

class GenerateHeisigMapTest extends AnyFlatSpec with Matchers{

  "test Heisig simp" should "be withig the first nine simplified" in {
    val testheisigSimp: Map[Grapheme, Int] = GenerateHeisigMap.heisigSimplified
    val heisigJundaAboveNine: SortedMap[String, List[OutputEntry]] = OutputOverlapObjects.jundaAboveNine
    
    var tempres: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (each <- heisigJundaAboveNine) {
      for (output <- each._2) {
        if (Grapheme.isGrapheme(output.chineseStr) && testheisigSimp.contains(Grapheme(output.chineseStr))) {
          tempres.add(output)
        }
      }
    }
    testheisigSimp.size shouldBe 3018
    tempres.size shouldBe 0
  }

  "test Heisig trad" should "be withig the first nine traditional" in {
    val testheisigTrad: Map[Grapheme, Int] = GenerateHeisigMap.heisigTraditional
    val heisigTzaiAboveNine: SortedMap[String, List[OutputEntry]] = OutputOverlapObjects.tzaiAboveNine

    var tempres: mutable.Set[(String, OutputEntry)] = mutable.Set[(String, OutputEntry)]()
    for (each <- heisigTzaiAboveNine) {
      for (output <- each._2) {
        if (Grapheme.isGrapheme(output.chineseStr) && testheisigTrad.contains(Grapheme(output.chineseStr))) {
          tempres.add((each._1, output))
        }
      }
    }
    testheisigTrad.size shouldBe 3035
    tempres.map(x => x._1).toSet shouldBe Set("jhxwwo", "nhxwwo")
    tempres.map(x => x._2.chineseStr).toSet shouldBe Set("騾")
  }
}
