package staticFileGenerators.IdsMap

import UtilityClasses.{Cluster, Grapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.io.Source

class GenerateNestedIdsMapTest extends AnyFlatSpec with Matchers {


  "The ids map" should "contain all conway characters" in {
    val result: HashMap[Grapheme, List[Cluster]] = GenerateNestedIdsMap.idsMap
    val allGraphemes: Set[Grapheme] = GenerateConwayCodes.conwaySet
    var missingGraphemes: ListBuffer[Grapheme] = ListBuffer()
    for (grap <- allGraphemes) {
      if (!result.contains(grap)) {
        missingGraphemes.addOne(grap)
      }
    }
    missingGraphemes.size shouldBe 0
  }


  "The GenerateNestedIdsMap function" should "correctly generate a map with replaced graphemes" in {
    val mapper = new GenerateNestedIdsMap()
    mapper.get(Grapheme("𠛷")) should not be empty

    var cluster1List = mapper.get(Grapheme("𠛷"))
    var cluster1 = cluster1List.head
    var cluster2 = new Cluster("⿲亻⿱⿱一一冫刂")
    cluster1 shouldEqual cluster2
  }


  "The mapIdsData function" should "correctly map the data from the file" in {
    val result = GenerateNestedIdsMap.idsMap

    // Test that the map is not empty
    result should not be empty

    // Assuming the first line of your file is:
    // "U+206F6 𠛶 ⿰⿱八文刂"
    // then '𠛶' should map to List("⿰⿱八文刂")
    val clusterlist = result(Grapheme("𠛶"))
    val firstCluster = clusterlist.head

    firstCluster shouldEqual Cluster("⿰⿱八文刂")

    // Add more tests as necessary...
  }

  it should "remove ascii characters from value strings" in {
    val result = GenerateNestedIdsMap.idsMap

    // Pick a key in your map which you know has an ASCII character in the value string
    // For example if '𠛹' maps to List("⿲亻⿱test"), then after removing ASCII characters
    // it should map to List("⿲亻⿱")
    val clusterlist = result(Grapheme("𠛹"))
    val firstCluster = clusterlist.head

    firstCluster shouldEqual Cluster("⿰𠕋刂")
  }
}