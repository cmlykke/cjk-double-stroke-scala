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

  it should "test that unrollBackslash wors" in {
    val test: String = AgraphemeToStrokeSet.unrollBackSlash("54(2511|3511|3541)(15|35|53)\\2")
    test shouldBe "54(2511|3511|3541)(15|35|53)(15|35|53)"

    val test2: String = AgraphemeToStrokeSet.unrollBackSlash("(122|1212|2112)1\\1112")
    test2 shouldBe "(122|1212|2112)1(122|1212|2112)1112"

    //(3511|3544)\1
    val test3: String = AgraphemeToStrokeSet.unrollBackSlash("(3511|3544)\\1")
    test3 shouldBe "(3511|3544)(3511|3544)"
  }
}
