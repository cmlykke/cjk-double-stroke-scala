package OutputTranslation

import UtilityClasses.{OutputEntry, OutputEntryFrequency}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap
import scala.collection.mutable.ListBuffer

object OutputOverlapObjects {

  def outputElevenEntryNestedListToString(code: List[String], 
                                          obj: List[Option[List[OutputEntry]]], 
                                          freq: OutputEntryFrequency): String = {
    var res = ListBuffer[String]()
    var indexRange = 0 to (code.length - 1)
    for (index <- indexRange) {
      val eachres: String = outputElevenEntryListToString(code(index), obj(index), freq)
      res.addOne(eachres)
    }
    return res.mkString("\n").trim
  }
  
  def outputElevenEntryListToString(prependStr: String, 
                                    input: Option[List[OutputEntry]], 
                                    freq: OutputEntryFrequency): String = {
    input match {
      case Some(entries) =>
        (prependStr + " " + entries.take(10).map(outputEntryToString(_, freq)).mkString(", ")).trim
      case None =>
        ""
    }
  }

  def outputEntryToString(input: OutputEntry, 
                          freq: OutputEntryFrequency): String = {
    val junda: Int = if (!input.jundaReverseOrder.isEmpty && !(input.jundaReverseOrder(0) == Int.MaxValue)) {input.jundaReverseOrder(0)} else {0}
    val tzai: Int = if (!input.tzaiReverseOrder.isEmpty && !(input.tzaiReverseOrder(0) == Int.MaxValue)) {input.tzaiReverseOrder(0)} else {0}
    val sinica: Int = if (!(input.sinicaOrd == Int.MaxValue)) {input.sinicaOrd} else {0}
    val bclu: Int = if (!(input.BCLUord == Int.MaxValue)) {input.BCLUord} else {0}
    if (freq == OutputEntryFrequency.Junda) {
      return input.chineseStr + " " + junda
    } else if (freq == OutputEntryFrequency.Tzai) {
      return input.chineseStr + " " + tzai
    } else if (freq == OutputEntryFrequency.BCLU) {
      return input.chineseStr + " " + bclu
    } else if (freq == OutputEntryFrequency.Sinica) {
      return input.chineseStr + " " + sinica
    } else {
      throw Exception("no known frequency system")
    }
  }

  val jundaAboveNine: SortedMap[String, List[OutputEntry]] =
    SortedMap(OutputSorting.mapFullJunda
      .filter(x => x._2.size > 9)
      .map { case (k, v) => k -> v.drop(9) } // Drop the first 9 elements first
      .filter { case (_, entries) =>
        entries.headOption.exists { entry =>
          entry.jundaReverseOrder.nonEmpty
        }
      }
      .toSeq: _*)

  val tzaiAboveNine: SortedMap[String, List[OutputEntry]] =
    SortedMap(OutputSorting.mapFullTzai
      .filter(x => x._2.size > 9)
      .map { case (k, v) => k -> v.drop(9) }
      .filter { case (_, entries) =>
        entries.headOption.exists { entry =>
          entry.tzaiReverseOrder.nonEmpty
        }
      }.toSeq: _*)

  // 3 and 5 code

  val BLCUjundaAboveNineThreeCode: List[(String, Int, String)] =
    jundaAboveNine.filter(y => y._1.size <= 3).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.BCLUoptionBD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val BCLU: Int = entries.headOption.flatMap(_.BCLUoptionBD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, BCLU, increasingStr))
    }.toList.sortBy { case (key, bclu, increasingStr) => (bclu, key, increasingStr) }

  val BLCUJundaAboveNineFiveCode: List[(String, Int, String)] =
    jundaAboveNine.filter(y => y._1.size == 5).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.BCLUoptionBD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val BCLU: Int = entries.headOption.flatMap(_.BCLUoptionBD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, BCLU, increasingStr))
    }.toList.sortBy { case (key, bclu, increasingStr) => (bclu, key, increasingStr) }

  val BLCUTzaiAboveNineThreeCode: List[(String, Int, String)] =
    tzaiAboveNine.filter(y => y._1.size <= 3).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.BCLUoptionBD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val BCLU: Int = entries.headOption.flatMap(_.BCLUoptionBD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, BCLU, increasingStr))
    }.toList.sortBy { case (key, bclu, increasingStr) => (bclu, key, increasingStr) }

  val BLCUTzaiAboveNineFiveCode: List[(String, Int, String)] =
    tzaiAboveNine.filter(y => y._1.size == 5).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.BCLUoptionBD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val BCLU: Int = entries.headOption.flatMap(_.BCLUoptionBD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, BCLU, increasingStr))
    }.toList.sortBy { case (key, bclu, increasingStr) => (bclu, key, increasingStr) }

  val sinicaJundaAboveNineThreeCode: List[(String, Int, String)] =
    jundaAboveNine.filter(y => y._1.size <= 3).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.sinicaOptionSD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val SINICA: Int = entries.headOption.flatMap(_.sinicaOptionSD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, SINICA, increasingStr))
    }.toList.sortBy { case (key, sinica, increasingStr) => (sinica, key, increasingStr) }

  val sinicaJundaAboveNineFiveCode: List[(String, Int, String)] =
    jundaAboveNine.filter(y => y._1.size == 5).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.sinicaOptionSD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val SINICA: Int = entries.headOption.flatMap(_.sinicaOptionSD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, SINICA, increasingStr))
    }.toList.sortBy { case (key, sinica, increasingStr) => (sinica, key, increasingStr) }

  val sinicaTzaiAboveNineThreeCode: List[(String, Int, String)] =
    tzaiAboveNine.filter(y => y._1.size <= 3).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.sinicaOptionSD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val SINICA: Int = entries.headOption.flatMap(_.sinicaOptionSD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, SINICA, increasingStr))
    }.toList.sortBy { case (key, sinica, increasingStr) => (sinica, key, increasingStr) }

  val sinicaTzaiAboveNineFiveCode: List[(String, Int, String)] =
    tzaiAboveNine.filter(y => y._1.size == 5).flatMap { case (key, entries) =>
      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.sinicaOptionSD
          .map(opt => if (opt.ordinal == Int.MaxValue) 0 else opt.ordinal).getOrElse(0)).mkString(", ").trim
      val SINICA: Int = entries.headOption.flatMap(_.sinicaOptionSD.map(_.ordinal)).getOrElse(Int.MaxValue)
      List((key, SINICA, increasingStr))
    }.toList.sortBy { case (key, sinica, increasingStr) => (sinica, key, increasingStr) }

  // 4 and 6 codes

  val singleTzaiAboveNineFourCode: List[(String, Int, String)] =
    tzaiAboveNine.filter(y => y._1.size == 4).flatMap { case (key, entries) =>
      //val increasingStr: String = entries
      //  .map(x => x.chineseStr + " " + x.tzaiReverseOrder.headOption.getOrElse(0)).mkString(", ").trim

      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.tzaiReverseOrder.headOption
          .map(opt => if (opt == Int.MaxValue) 0 else opt).getOrElse(0)).mkString(", ").trim

      val FREQ: Int = entries.headOption.flatMap(_.tzaiReverseOrder.headOption).getOrElse(Int.MaxValue)
      List((key, FREQ, increasingStr))
    }.toList.sortBy { case (key, freq, increasingStr) => (freq, key, increasingStr) }

  val singleTzaiAboveNineSixCode: List[(String, Int, String)] =
      tzaiAboveNine.filter(y => y._1.size == 6).flatMap { case (key, entries) =>
        //val increasingStr: String = entries
        //  .map(x => x.chineseStr + " " + x.tzaiReverseOrder.headOption.getOrElse(0)).mkString(", ").trim

        val increasingStr: String = entries
          .map(x => x.chineseStr + " " + x.tzaiReverseOrder.headOption
            .map(opt => if (opt == Int.MaxValue) 0 else opt).getOrElse(0)).mkString(", ").trim

        val FREQ: Int = entries.headOption.flatMap(_.tzaiReverseOrder.headOption).getOrElse(Int.MaxValue)
        List((key, FREQ, increasingStr))
      }.toList.sortBy { case (key, freq, increasingStr) => (freq, key, increasingStr) }

  val singleJundaAboveNineFourCode: List[(String, Int, String)] =
    jundaAboveNine.filter(y => y._1.size == 4).flatMap { case (key, entries) =>
      //val increasingStr: String = entries
      //  .map(x => x.chineseStr + " " + x.jundaReverseOrder.headOption.getOrElse(0)).mkString(", ").trim

      val increasingStr: String = entries
        .map(x => x.chineseStr + " " + x.jundaReverseOrder.headOption
          .map(opt => if (opt == Int.MaxValue) 0 else opt).getOrElse(0)).mkString(", ").trim

      val FREQ: Int = entries.headOption.flatMap(_.jundaReverseOrder.headOption).getOrElse(Int.MaxValue)
      List((key, FREQ, increasingStr))
    }.toList.sortBy { case (key, freq, increasingStr) => (freq, key, increasingStr) }

  val singleJundaAboveNineSixCode: List[(String, Int, String)] =
      jundaAboveNine.filter(y => y._1.size == 6).flatMap { case (key, entries) =>
        //val increasingStr: String = entries
        //  .map(x => x.chineseStr + " " + x.jundaReverseOrder.headOption.getOrElse(0)).mkString(", ").trim
        val increasingStr: String = entries
          .map(x => x.chineseStr + " " + x.jundaReverseOrder.headOption
            .map(opt => if (opt == Int.MaxValue) 0 else opt).getOrElse(0)).mkString(", ").trim
        val FREQ: Int = entries.headOption.flatMap(_.jundaReverseOrder.headOption).getOrElse(Int.MaxValue)
        List((key, FREQ, increasingStr))
      }.toList.sortBy { case (key, freq, increasingStr) => (freq, key, increasingStr) }

  def singleStrFromList(input: List[(String, Int, String)]): String = {
    val res = new StringBuilder
    for ((str1, num, str3) <- input) {
      val splitStr = str3.split(",")
      val minires = new StringBuilder
      for (j <- splitStr.indices.take(10)) {
        minires.append(splitStr(j)).append(",")
      }
      if (minires.nonEmpty && minires.last == ',') {
        minires.setLength(minires.length - 1)
      }
      res.append(s"$str1 $num $minires")
      res.append("\n")
    }
    res.toString().trim
  }

}
