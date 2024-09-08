package staticFileGenerators.IdsMap

import UtilityClasses.{Cluster, Grapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.HashMap
import scala.io.Source

class GenerateNestedIdsMapTest extends AnyFlatSpec with Matchers {

  "The GenerateNestedIdsMap function" should "correctly generate a map with replaced graphemes" in {
    val mapper = new GenerateNestedIdsMap()
    mapper.get(Grapheme("𠛷")) should not be empty

    var cluster1List = mapper.get(Grapheme("𠛷"))
    var cluster1 = cluster1List.head
    var cluster2 = new Cluster("⿲亻⿱⿱一一冫刂")
    cluster1 shouldEqual cluster2
  }
}