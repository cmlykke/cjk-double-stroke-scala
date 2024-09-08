package staticFileGenerators.cedictMap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes

class cedictMapTest extends AnyFlatSpec with Matchers{

  
  //GenerateCedictMap


  "cedict" should "be generated" in {

    val tes = new GenerateCedictMap()
    val tes2 = tes.generateList()
    val test = ""
    /*
    val mapper = new GenerateConwayCodes()
    val allChars = mapper.getConwayMap
    allChars.keys.size shouldEqual 28100*/

  }
  
}
