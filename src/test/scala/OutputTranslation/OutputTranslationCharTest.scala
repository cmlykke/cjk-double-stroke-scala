package OutputTranslation

import UtilityClasses.{CedictEntry, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

class OutputTranslationCharTest extends AnyFlatSpec with Matchers {
  /*
  it shoud "translate conway to OutputEntry with the same criteria as cedict" in {
    xxx - write the needed code in OutputTranslation
  }*/

  it should "test individual character codes" in {
    val cedictRaw: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
    val cedictres: Set[OutputEntry] = OutputTranslation.outputCedict

    //U+9019	這^	(1|4)111251(454|4454|4554)
    val thisChar: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "這").toList
    thisChar.size shouldBe 1
    val thisCharEntry = thisChar(0)
    thisCharEntry.codes shouldBe Set("iwsz","iqtz","iqsz", "iws", "iqt", "iqs", "hhxlat", "hhxlsz", "thxlqt", "thxlat", "hhxlqt", "thxlsz");

    //U+4E6A	乪	525121
    val unChar: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "乪").toList
    unChar.size shouldBe 1
    val unCharEntry = unChar(0)
    unCharEntry.codes shouldBe Set("fgnz", "fgnzzz")

    //U+5F4E	彎^	(1|4)111251(554234|554444)\2515
    val bendChar: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "彎").toList
    bendChar.size shouldBe 1
    val bendCharEntry = bendChar(0)
    bendCharEntry.codes shouldBe Set("thxmsm", "hhxmsm", "iawm", "iarm")

    val roughChar: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "碌").toList
    roughChar.size shouldBe 1
    val roughCharEntry = roughChar(0)
    roughCharEntry.codes shouldBe Set("kxmgco", "kxmhco", "kxmo")

    val divideChar: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "解").toList
    divideChar.size shouldBe 1
    val divideCharEntry = divideChar(0)
    divideCharEntry.codes shouldBe Set("ppjmij", "pphxij", "pphj", "ppjj")
/*
    val fakeChar: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "偽").toList
    fakeChar.size shouldBe 1
    val fakeCharEntry = roughChar(0)
    fakeCharEntry.codes shouldBe Set("zh")
*/
  }
/*
  def processString(input: String): Set[String] = {
    // You can implement the logic here
    ???
  }*/



}
