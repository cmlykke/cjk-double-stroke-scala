package UtilityClasses

import staticFileGenerators.Academiasinica.{GenerateSinicaMap, SinicaData}
import staticFileGenerators.BLCUmap.{BLCUData, GenerateBLCUmap}

import scala.collection.mutable.{LinkedHashMap, ListBuffer}

class OutputEntry(inputChineseStr: String,
                  inputMeaning: String,
                  inputPronounciation: String,
                  inputTradSimp: String,
                  inpjundaReverseOrder: List[Grapheme],
                  inptzaiReverseOrder: List[Grapheme],
                  inpcodes: Set[String]) {
  val chineseStr: String = inputChineseStr
  val meaning: String = inputMeaning
  val pron: String = inputPronounciation
  val tradSimp: String = inputTradSimp
  val codes: Set[String] = inpcodes
  //val jundaReverseOrder: List[Grapheme] = inpjundaReverseOrder
  //val tzaiReverseOrder: List[Grapheme] = inptzaiReverseOrder
  //val sinicaOption: Option[SinicaData] = OutputEntry.sinicaMap.get(inputChineseStr)
  //val BCLUoption: Option[BLCUData] = OutputEntry.BCLUmap.get(inputChineseStr)
  val jundaReverseOrderG: List[Grapheme] = inpjundaReverseOrder
  val jundaReverseOrder: List[Int] = generateJundaOrdFromList(jundaReverseOrderG, inputChineseStr)
  val tzaiReverseOrderG: List[Grapheme] = inptzaiReverseOrder
  val tzaiReverseOrder: List[Int] = generateTzaiOrdFromList(tzaiReverseOrderG, inputChineseStr)
  
  val sinicaOptionSD: Option[SinicaData] = OutputEntry.sinicaMap.get(inputChineseStr)
  val sinicaOrd: Int = if (sinicaOptionSD.isDefined) sinicaOptionSD.get.ordinal else Int.MaxValue
  val BCLUoptionBD: Option[BLCUData] = OutputEntry.BCLUmap.get(inputChineseStr)
  val BCLUord: Int = if (BCLUoptionBD.isDefined) BCLUoptionBD.get.ordinal else Int.MaxValue
  val BCLUdub: Double = if (BCLUoptionBD.isDefined) BCLUoptionBD.get.frequency else 0

  
  
  private def generateTzaiOrdFromList(inp: List[Grapheme], inputChineseStr: String): List[Int] = {
    if (inputChineseStr == "称") {
      val test = ""
    }
    var res = ListBuffer[Int]()
    val tempres = inp.map(x => generateTzaiOrd(x)).toList.sorted.reverse
    return tempres
  }

  private def generateTzaiOrd(inp: Grapheme): Int = {
    if (inp.tzai.isDefined) {
      return inp.tzai.get.ordinal
    } else {
      Int.MaxValue
    }
  }
  
  private def generateJundaOrdFromList(inp: List[Grapheme], inputChineseStr: String): List[Int] = {
    var res = ListBuffer[Int]()
    val tempres = inp.map(x => generateJundaOrd(x)).toList.sorted.reverse
    return tempres
  }

  private def generateJundaOrd(inp: Grapheme): Int = {
    if (inp.junda.isDefined) {
      return inp.junda.get.ordinal
    } else {
      Int.MaxValue
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case g: OutputEntry => g.chineseStr == this.chineseStr
    case _           => false
  }

  override def hashCode(): Int = chineseStr.hashCode
}

object OutputEntry {
  val sinicaMap: LinkedHashMap[String, SinicaData] = GenerateSinicaMap.sinicaMap
  val BCLUmap: LinkedHashMap[String, BLCUData] = GenerateBLCUmap.blcuMap
}
