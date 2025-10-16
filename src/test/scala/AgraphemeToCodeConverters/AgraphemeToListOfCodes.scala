package AgraphemeToCodeConverters

import Atypes.Agrapheme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgraphemeToListOfCodes extends AnyFlatSpec with Matchers {

  it should "test that grapheme to code set works" in {
    val codeTest1: Set[String] = AgraphemeToStrokeSet.generateStrokeSet(Agrapheme("言"))
    codeTest1 shouldEqual Set("1111251", "4111251")

    val codeTest1b: Set[String] = AgraphemeToStrokeSet.generateStrokeSet(Agrapheme("誠"))
    codeTest1b shouldEqual Set("4111251135543", "4111251135534", "1111251135543", "1111251135534")

    val codeTest2: Set[String] = AgraphemeToStrokeSet.expandAlt("(1|4)111251")
    codeTest2 shouldEqual Set("1111251", "4111251")

  }
}
