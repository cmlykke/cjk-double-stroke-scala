package staticFileGenerators.blcumap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Academiasinica.GenerateSinicaMap
import staticFileGenerators.BLCUmap.GenerateBLCUmap

class GenerateBLCUmapTest extends AnyFlatSpec with Matchers{

  "testAllCedict" should "contain only a fraction of the characters in conway" in {

    val blcumap = GenerateBLCUmap.blcuMap
    blcumap.size shouldBe 1048570

    val test = ""
  }
}
