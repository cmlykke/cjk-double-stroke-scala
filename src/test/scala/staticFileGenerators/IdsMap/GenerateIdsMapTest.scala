package staticFileGenerators.IdsMap

import UtilityClasses.{Cluster, Grapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.HashMap
import scala.io.Source

class GenerateIdsMapTest extends AnyFlatSpec with Matchers {

  "The mapIdsData function" should "correctly map the data from the file" in {
    val mapper = new GenerateIdsMap()
    val result = mapper.mapIdsData()

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
    val mapper = new GenerateIdsMap()
    val result = mapper.mapIdsData()

    // Pick a key in your map which you know has an ASCII character in the value string
    // For example if '𠛹' maps to List("⿲亻⿱test"), then after removing ASCII characters
    // it should map to List("⿲亻⿱")
    val clusterlist = result(Grapheme("𠛹"))
    val firstCluster = clusterlist.head

    firstCluster shouldEqual Cluster("⿰𠕋刂")
  }

}
//𠛷