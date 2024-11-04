package OutputTranslation

import GenerateOutput.GenerateOutputStrings
import UtilityClasses.{Grapheme, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class CharacterCount extends AnyFlatSpec with Matchers {

  val generate = new GenerateOutputStrings()
  val outputLinesJ: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
  val outJStr: Set[String] = outputLinesJ
    .map(x => x.split("\t"))
    .map(x => x(1)).toSet
  val outputLinesT: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullTzai)
  val outTStr: Set[String] = outputLinesT
    .map(x => x.split("\t"))
    .map(x => x(1)).toSet

  it should "check the number of different character" in {

    val singleJunda = outJStr
      .filter(z => Grapheme.isGrapheme(z)).toSet
    val singleTzai = outTStr
      .filter(z => Grapheme.isGrapheme(z)).toSet
    //It contains 29.512 different single characters, and 179.799 multi-character words
    singleJunda.size shouldBe 29512
    singleTzai.size shouldBe 29512
  }


  it should "check the number of different words" in {

    val multiCharacterJ = outJStr
      .filter(z => !Grapheme.isGrapheme(z)).toSet
    val multiCharacterT = outTStr
      .filter(z => !Grapheme.isGrapheme(z)).toSet
    //It contains 29.512 different single characters, and 179.799 multi-character words
    multiCharacterJ.size shouldBe 179799
    multiCharacterT.size shouldBe 179799
  }

  it should "check the number of different two-character words" in {

    val twoCharacterJ = outJStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size == 2).toSet
    val twoCharacterT = outTStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size == 2).toSet

    twoCharacterJ.size shouldBe 93573
    twoCharacterT.size shouldBe 93573
  }

  it should "check the number of different multi-character words" in {

    val threePlusCharacterJ = outJStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 2).toSet
    val threePlusCharacterT = outTStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 2).toSet

    threePlusCharacterJ.size shouldBe 86226
    threePlusCharacterT.size shouldBe 86226
  }

}
