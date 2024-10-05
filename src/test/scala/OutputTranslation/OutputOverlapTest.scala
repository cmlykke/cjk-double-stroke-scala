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

  //sinicaCharsBeyond9
  val sinicaJundaAboveNineFourCode: List[(String, String, Int)] =
    jundaAboveNine.filter(y => y._1.size == 4).flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        sinicaOption: Int = firstEntry.sinicaOption.map(_.ordinal).getOrElse(999999)//firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        //jundaData <- firstGrapheme.junda
      } yield (key, firstEntry.chineseStr, sinicaOption)
    }.toList.sortBy { case (key, chineseStr, sinicaOption) => (sinicaOption, key, chineseStr) }

  val sinicaJundaAboveNineFiveCode: List[(String, String, Int)] =
    jundaAboveNine.filter(y => y._1.size == 5).flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        sinicaOption: Int = firstEntry.sinicaOption.map(_.ordinal).getOrElse(999999)
        //firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        //jundaData <- firstGrapheme.junda
      } yield (key, firstEntry.chineseStr, sinicaOption)
    }.toList.sortBy { case (key, chineseStr, sinicaOption) => (sinicaOption, key, chineseStr) }

  val sinicaTzaiAboveNineFourCode: List[(String, String, Int)] =
      tzaiAboveNine.filter(y => y._1.size == 4).flatMap { case (key, entries) =>
        for {
          firstEntry <- entries.headOption
          sinicaOption: Int = firstEntry.sinicaOption.map(_.ordinal).getOrElse(999999)
          //firstGrapheme <- firstEntry.jundaReverseOrder.headOption
          //jundaData <- firstGrapheme.junda
        } yield (key, firstEntry.chineseStr, sinicaOption)
      }.toList.sortBy { case (key, chineseStr, sinicaOption) => (sinicaOption, key, chineseStr) }


  val sinicaTzaiAboveNineFiveCode: List[(String, String, Int)] =
    tzaiAboveNine.filter(y => y._1.size == 5).flatMap { case (key, entries) =>
      for {
        firstEntry <- entries.headOption
        sinicaOption: Int = firstEntry.sinicaOption.map(_.ordinal).getOrElse(999999)
        //firstGrapheme <- firstEntry.jundaReverseOrder.headOption
        //jundaData <- firstGrapheme.junda
      } yield (key, firstEntry.chineseStr, sinicaOption)
    }.toList.sortBy { case (key, chineseStr, sinicaOption) => (sinicaOption, key, chineseStr) }


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

  
  it should "check that junda sinica is low, ie. most frequent sinica words a below 9" in {
    val sinicafour = sinicaJundaAboveNineFourCode
    val sinicafive = sinicaJundaAboveNineFiveCode
    sinicafour.length shouldBe 60
    sinicafive.length shouldBe 191
    val test = ""
  }

  it should "check that tzai sinica is low, ie. most frequent sinica words a below 9" in {
    val sinicafour = sinicaTzaiAboveNineFourCode
    val sinicafive = sinicaTzaiAboveNineFiveCode
    sinicafour.length shouldBe 117
    sinicafive.length shouldBe 185
    val test = ""
  }

  it should "check 6 codes" in {
    val jundaTest = sixJundaPair
    val tzaiTest = sixTzaiPair
    jundaTest.size shouldBe 3
    tzaiTest.size shouldBe 18
    val test = ""
  }

  it should "check 5 codes" in {
    val jundaTest = fiveJundaPair
    val tzaiTest = fiveTzaiPair
    jundaTest(0)._2 shouldBe 1168
    tzaiTest(0)._2 shouldBe 621
    
    val jundaStr = tuppleToStr(jundaTest)
    val tzaiStr = tuppleToStr(tzaiTest)
    
    jundaStr shouldBe """(jhozz, 1168, 丁夫)
                        |(joxht, 1169, 赵县)
                        |(xhwkt, 1222, 冒头)
                        |(joxho, 1272, 茶具)
                        |(wowkt, 1721, 浅海)
                        |(joxhh, 1725, 茶晶)
                        |(xhxhh, 1725, 晶晶)
                        |(jhxht, 1838, 霸县)
                        |(dofhz, 2187, 猴子)
                        |(jgjjo, 2242, 坑蒙)""".stripMargin
    
    tzaiStr shouldBe """(joxjy, 621, 救國)
                       |(woxjo, 863, 寶典)
                       |(joxho, 946, 載具)
                       |(koxho, 1032, 成縣)
                       |(jokxh, 1134, 頂面)
                       |(jojno, 1160, 歡聚)
                       |(jgjjo, 1167, 西藥)
                       |(jojgo, 1193, 東歐)
                       |(joozz, 1198, 賢人)
                       |(jsxho, 1246, 零數)""".stripMargin
    
    val test = ""
  }

  it should "check 4 code overlap" in {
    val jundaTest = fourCodeCodeJundaPair
    val tzaiTest = fourCodeCodeTzaiPair
    jundaTest(0)._2 shouldBe 5105
    tzaiTest(0)._2 shouldBe 5412

    val jundaStr = tuppleToStr(jundaTest)
    val tzaiStr = tuppleToStr(tzaiTest)

    jundaStr shouldBe """(xhjo, 5105, 題)
                        |(jnho, 5209, 聩)
                        |(kxho, 5772, 砹)
                        |(ejoo, 5898, 糇)
                        |(yhso, 5941, 锿)
                        |(yhds, 6247, 锾)
                        |(pxjh, 6310, 鲭)
                        |(pxjo, 6451, 鳜)
                        |(yhgo, 6536, 铗)
                        |(jjko, 6578, 趱)""".stripMargin

    tzaiStr shouldBe """(jnxo, 5412, 鞅)
                       |(kxho, 5477, 饜)
                       |(xjgo, 5724, 嘳)
                       |(xhjo, 5811, 暪)
                       |(wjgo, 5821, 漯)
                       |(wjgh, 6234, 澠)
                       |(jjjh, 6434, 莤)
                       |(pxjo, 6470, 鯕)
                       |(djgo, 6856, 嬠)
                       |(jjxo, 6869, 藈)""".stripMargin
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
