package OutputTranslation

import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap

class OutputOverlapTest extends AnyFlatSpec with Matchers {

  val jundaAboveNine: SortedMap[String, List[OutputEntry]] =
    SortedMap(OutputSorting.mapFullJunda
      .filter(x => x._2.size > 9)
      .map { case (k, v) => k -> v.drop(9) } // Drop the first 9 elements first
      .filter { case (_, entries) =>
        entries.headOption.exists { entry =>
          entry.jundaReverseOrder.nonEmpty &&
            entry.jundaReverseOrder.headOption.exists(_.junda.isDefined)
        }
      }
      .toSeq: _*)


  val tzaiAboveNine: SortedMap[String, List[OutputEntry]] =
    SortedMap(OutputSorting.mapFullTzai
      .filter(x => x._2.size > 9)
      .map { case (k, v) => k -> v.drop(9) }
      .filter { case (_, entries) =>
        entries.headOption.exists { entry =>
          entry.tzaiReverseOrder.nonEmpty &&
            entry.tzaiReverseOrder.headOption.exists(_.tzai.isDefined)
        }
      }.toSeq: _*)

  // 4 codes - or less

  val fourCodeOrLessJunda: SortedMap[String, List[OutputEntry]] = jundaAboveNine.filter(x => x._1.size <= 4)
  val fourCodeCodeJundaPair: List[(String, Int)] =
    fourCodeOrLessJunda.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        jundaData <- firstGrapheme.junda
      } yield (key, jundaData.ordinal)
    }.toList.sortBy { case (key, ordinal) => (ordinal, key) }

  val fourCodeOrLessTzai: SortedMap[String, List[OutputEntry]] = tzaiAboveNine.filter(x => x._1.size <= 4)
  val fourCodeCodeTzaiPair: List[(String, Int)] =
    fourCodeOrLessTzai.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.tzaiReverseOrder.headOption
        tzaiData <- firstGrapheme.tzai
      } yield (key, tzaiData.ordinal)
    }.toList.sortBy { case (key, ordinal) => (ordinal, key) }

  // five codes

  val fiveJunda: SortedMap[String, List[OutputEntry]] = jundaAboveNine.filter(x => x._1.size == 5)
  val fiveJundaPair: List[(String, Int, String)] =
    fiveJunda.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        jundaData <- firstGrapheme.junda
      } yield (key, jundaData.ordinal, firstEntry.chineseStr)
    }.toList.sortBy { case (key, ordinal, chinese) => (ordinal, key, chinese) }

  val fiveTzai: SortedMap[String, List[OutputEntry]] = tzaiAboveNine.filter(x => x._1.size == 5)
  val fiveTzaiPair: List[(String, Int, String)] =
    fiveTzai.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.tzaiReverseOrder.headOption
        tzaiData <- firstGrapheme.tzai
      } yield (key, tzaiData.ordinal, firstEntry.chineseStr)
    }.toList.sortBy { case (key, ordinal, chinese) => (ordinal, key, chinese) }

  val sixJunda: SortedMap[String, List[OutputEntry]] = jundaAboveNine.filter(x => x._1.size == 6)
  val sixJundaPair: List[(String, Int, String)] =
    sixJunda.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        jundaData <- firstGrapheme.junda
      } yield (key, jundaData.ordinal, firstEntry.chineseStr)
    }.toList.sortBy { case (key, ordinal, chinese) => (ordinal, key, chinese) }

  val sixTzai: SortedMap[String, List[OutputEntry]] = tzaiAboveNine.filter(x => x._1.size == 6)
  val sixTzaiPair: List[(String, Int, String)] =
    sixTzai.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.tzaiReverseOrder.headOption
        tzaiData <- firstGrapheme.tzai
      } yield (key, tzaiData.ordinal, firstEntry.chineseStr)
    }.toList.sortBy { case (key, ordinal, chinese) => (ordinal, key, chinese) }

  val test = ""


  it should "check 6 codes" in {
    val jundaTest = sixJundaPair
    val tzaiTest = sixTzaiPair
    jundaTest.size shouldBe 0
    tzaiTest.size shouldBe 0
    val test = ""
  }

  it should "check 5 codes" in {
    val jundaTest = fiveJundaPair
    val tzaiTest = fiveTzaiPair
    jundaTest(0)._2 shouldBe 1168
    tzaiTest(0)._2 shouldBe 621
    val test = ""
  }

  it should "check 4 code overlap" in {
    val jundaTest = fourCodeCodeJundaPair
    val tzaiTest = fourCodeCodeTzaiPair
    jundaTest(0)._2 shouldBe 5105
    val junda2 = OutputSorting.mapFullJunda.get("ngfo").get
    val junda3 = OutputSorting.mapFullJunda.get("agnj").get
    tzaiTest(0)._2 shouldBe 5412
    val test = ""
  }


}
