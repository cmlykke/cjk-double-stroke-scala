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
    .map(x => x.split("\\s+")).flatten
    .filter(_.exists(ch => ch > 127)).toSet

  val outputLinesT: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullTzai)
  val outTStr: Set[String] = outputLinesT
    .map(x => x.split("\\s+")).flatten
    .filter(_.exists(ch => ch > 127)).toSet

  it should "check the number of different character" in {

    val singleJunda = outJStr
      .filter(z => Grapheme.isGrapheme(z)).toSet
    val singleTzai = outTStr
      .filter(z => Grapheme.isGrapheme(z)).toSet
    //It contains 29.482 different single characters, and 179.767 multi-character words
    singleJunda.size shouldBe 29482
    singleTzai.size shouldBe 29482
  }


  it should "check the number of different words" in {

    val singleJunda = outJStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 1).toSet
    val singleTzai = outTStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 1).toSet
    //It contains 29.482 different single characters, and 179.767 multi-character words
    singleJunda.size shouldBe 179767
    singleTzai.size shouldBe 179767
  }

  it should "check the number of different two-character words" in {

    val singleJunda = outJStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size == 2).toSet
    val singleTzai = outTStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size == 2).toSet

    singleJunda.size shouldBe 93553
    singleTzai.size shouldBe 93553
  }

  it should "check the number of different multi-character words" in {

    val singleJunda = outJStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 2).toSet
    val singleTzai = outTStr
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 2).toSet

    singleJunda.size shouldBe 86214
    singleTzai.size shouldBe 86214
  }

}
