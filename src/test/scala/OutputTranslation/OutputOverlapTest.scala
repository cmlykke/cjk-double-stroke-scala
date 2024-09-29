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
  val fourCodeCodeJundaPair: List[(String, Int, String)] =
    fourCodeOrLessJunda.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        jundaData <- firstGrapheme.junda
      } yield (key, jundaData.ordinal, firstGrapheme.char)
    }.toList.sortBy { case (key, ordinal, chinese) => (ordinal, key, chinese) }

  val fourCodeOrLessTzai: SortedMap[String, List[OutputEntry]] = tzaiAboveNine.filter(x => x._1.size <= 4)
  val fourCodeCodeTzaiPair: List[(String, Int, String)] =
    fourCodeOrLessTzai.flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        firstGrapheme <- firstEntry.tzaiReverseOrder.headOption
        tzaiData <- firstGrapheme.tzai
      } yield (key, tzaiData.ordinal, firstGrapheme.char)
    }.toList.sortBy { case (key, ordinal, chineseStr) => (ordinal, key, chineseStr) }

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
    
    val jundaStr = tuppleToStr(jundaTest)
    val tzaiStr = tuppleToStr(tzaiTest)
    
    jundaStr shouldBe """(fgozz, 1168, 丁夫)
                        |(fongt, 1169, 赵县)
                        |(ngwdt, 1222, 冒头)
                        |(fongo, 1272, 茶具)
                        |(wowdt, 1721, 浅海)
                        |(fongg, 1725, 茶晶)
                        |(ngngg, 1725, 晶晶)
                        |(fgngt, 1838, 霸县)
                        |(kojgz, 2187, 猴子)
                        |(fhffo, 2242, 坑蒙)""".stripMargin
    
    tzaiStr shouldBe """(fonfy, 621, 救國)
                       |(wonfo, 863, 寶典)
                       |(fongo, 946, 載具)
                       |(dongo, 1032, 成縣)
                       |(fodng, 1134, 頂面)
                       |(fofxo, 1160, 歡聚)
                       |(fhffo, 1167, 西藥)
                       |(fofho, 1193, 東歐)
                       |(foozz, 1198, 賢人)
                       |(flngo, 1246, 零數)""".stripMargin
    
    val test = ""
  }

  it should "check 4 code overlap" in {
    val jundaTest = fourCodeCodeJundaPair
    val tzaiTest = fourCodeCodeTzaiPair
    jundaTest(0)._2 shouldBe 5105
    tzaiTest(0)._2 shouldBe 5412

    val jundaStr = tuppleToStr(jundaTest)
    val tzaiStr = tuppleToStr(tzaiTest)

    jundaStr shouldBe """(ngfo, 5105, 題)
                        |(fxgo, 5209, 聩)
                        |(dngo, 5772, 砹)
                        |(efoo, 5898, 糇)
                        |(yglo, 5941, 锿)
                        |(ygkl, 6247, 锾)
                        |(pnfg, 6310, 鲭)
                        |(pnfo, 6451, 鳜)
                        |(ygho, 6536, 铗)
                        |(ffdo, 6578, 趱)""".stripMargin

    tzaiStr shouldBe """(fxno, 5412, 鞅)
                       |(dngo, 5477, 饜)
                       |(nfho, 5724, 嘳)
                       |(ngfo, 5811, 暪)
                       |(wfho, 5821, 漯)
                       |(wfhg, 6234, 澠)
                       |(fffg, 6434, 莤)
                       |(pnfo, 6470, 鯕)
                       |(kfho, 6856, 嬠)
                       |(ffno, 6869, 藈)""".stripMargin
    val test = ""
  }


  private def tuppleTwoToStr(input: List[(String, Int)]): String = {
    if (input.size > 9) {
      val res  = input.take(10).map(x =>  "(" + x._1 +", " + x._2 + ")").mkString("\r\n").trim
      return res
    } else if (input.size > 0) {
      val res = input.map(x => "(" + x._1 + ", " + x._2 + ")").mkString("\r\n").trim
      return res
    } else {
      return "None"
    }
  }
  
  private def tuppleToStr(input: List[(String, Int, String)]): String = {
    if (input.size > 9) {
      val res  = input.take(10).map(x =>  "(" + x._1 +", " + x._2 + ", " + x._3 + ")").mkString("\r\n").trim
      return res
    } else if (input.size > 0) {
      val res = input.map(x => "(" + x._1 + ", " + x._2 + ", " + x._3 + ")").mkString("\r\n").trim
      return res
    } else {
      return "None"
    }
  }


}
