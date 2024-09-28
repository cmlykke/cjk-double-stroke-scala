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

  val fourCodeJunda: SortedMap[String, List[OutputEntry]] = jundaAboveNine.filter(x => x._1.size == 4)
  val fourCodeCodeJundaPair: List[(String, Int)] =
    fourCodeJunda.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        jundaData <- firstGrapheme.junda
      } yield (key, jundaData.ordinal)
    }.toList.sortBy { case (key, ordinal) => (ordinal, key) }
  val test = ""

  it should "check 4 code overlap" in {
    val kojgCode = jundaAboveNine.get("kojg")
    val ngjgCode = jundaAboveNine.get("ngjg")
    kojgCode.get.size shouldBe 12
    ngjgCode.get.size shouldBe 6
    val test = ""
  }


}
