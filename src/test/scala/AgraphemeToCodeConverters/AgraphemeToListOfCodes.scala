package AgraphemeToCodeConverters

import Atypes.Agrapheme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgraphemeToListOfCodes extends AnyFlatSpec with Matchers {

  it should "test that grapheme to code set works" in {
    val codeTest1: Set[String] = AgraphemeToStrokeSet.generateStrokeSet(Agrapheme("言"))
    codeTest1 shouldEqual Set("1111251", "4111251")
  }
}
