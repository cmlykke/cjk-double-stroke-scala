package staticFileGenerators.academiasinica

import UtilityClasses.{CedictEntry, Grapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap
import staticFileGenerators.Academiasinica.GenerateSinicaMap

import scala.collection.mutable

class sinicaMapTest extends AnyFlatSpec with Matchers{

  "testAllCedict" should "contain only a fraction of the characters in conway" in {
    
    val sinicaMap = GenerateSinicaMap.sinicaMap
    sinicaMap.size shouldBe 8272
    
    val test = ""
  }
}
