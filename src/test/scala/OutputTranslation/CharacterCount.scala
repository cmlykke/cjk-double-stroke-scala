package OutputTranslation

import UtilityClasses.{Grapheme, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class CharacterCount extends AnyFlatSpec with Matchers {

  it should "check the number of different character" in {
    val junda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val tzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai

    val singleJunda = junda.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.isGrapheme(z)).toSet
    val singleTzai = tzai.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.isGrapheme(z)).toSet

    singleJunda.size shouldBe 28320
    singleTzai.size shouldBe 28320
  }


  it should "check the number of different words" in {
    val junda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val tzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai

    val singleJunda = junda.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 1).toSet
    val singleTzai = tzai.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 1).toSet

    singleJunda.size shouldBe 179780
    singleTzai.size shouldBe 179780
  }

  it should "check the number of different two-character words" in {
    val junda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val tzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai

    val singleJunda = junda.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.splitIntoGraphemes(z).size == 2).toSet
    val singleTzai = tzai.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.splitIntoGraphemes(z).size == 2).toSet

    singleJunda.size shouldBe 93554
    singleTzai.size shouldBe 93554
  }

  it should "check the number of different multi-character words" in {
    val junda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val tzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai

    val singleJunda = junda.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 2).toSet
    val singleTzai = tzai.values.flatten.map(x => x.chineseStr)
      .filter(z => Grapheme.splitIntoGraphemes(z).size > 2).toSet

    singleJunda.size shouldBe 86226
    singleTzai.size shouldBe 86226
  }

}
