package Atypes

import Adatasources.ManualData.AtextType
import Asingletons.AsingletonsForTests
import UtilityClasses.Grapheme

import scala.collection.immutable
import scala.jdk.StreamConverters.*
import scala.math.Ordering

class AsortingObject(inputText: String,
                     criteria: AsortingCriteria,
                     textType: AtextType) extends Ordered[AsortingObject] {

  val graphemes: List[String] = Grapheme.splitIntoGraphemes(inputText)
  val sortingString: String = AsortingObject.generateSortingString(inputText, graphemes, criteria, textType)
  val cedictType: AtextType = AsortingObject.findTextType(inputText)

  def compare(that: AsortingObject): Int = {
    return this.sortingString.compare(that.sortingString)
  }
}

object AsortingObject {

  def findTextType(inputText: String): AtextType = {
    val hasConwayGrapheme: Boolean = Grapheme.splitIntoGraphemes(inputText)
      .exists { x =>
        AsingletonsForTests.conwaymap.contains(Agrapheme(x))
      }
    if (!hasConwayGrapheme) {
      return AtextType.Neither
    }

    //val graphemes: Set[Grapheme] = Grapheme.splitIntoGraphemes(inputText)
    val cedictSimp: Boolean = AsingletonsForTests.cedict.simplifiedAllHanItems.contains(AcedictEntry(inputText))
    val cedictTrad: Boolean = AsingletonsForTests.cedict.traditionalAllHanItems.contains(AcedictEntry(inputText))
    val cedictResult = getBooleanResult(cedictSimp, cedictTrad)
    if (!(cedictResult == AtextType.Neither)) {
      return cedictResult
    }

    val bcluSimp: Boolean = AsingletonsForTests.cedict.simplifiedAllHanItems.contains(AcedictEntry(inputText))
    val sinecaTrad: Boolean = AsingletonsForTests.cedict.traditionalAllHanItems.contains(AcedictEntry(inputText))
    val wordFreqResult = getBooleanResult(bcluSimp, sinecaTrad)
    if (!(wordFreqResult == AtextType.Neither)) {
      return wordFreqResult
    }

    return AtextType.OtherHanCharacter
  }

  private def getBooleanResult(simplified: Boolean, Traditional: Boolean): AtextType = {
    if (simplified && Traditional) {
      return AtextType.BothSimplifiedAndTraditional
    } else if (simplified) {
      return AtextType.Simplified
    } else if (Traditional) {
      return AtextType.Traditional
    } else {
      return AtextType.Neither
    }
  }

  def generateSortingString(inputText: String,
                            graphs:  List[String],
                            criteria: AsortingCriteria,
                            textType: AtextType): String = {
    var output: String = ""

    if (criteria.code > 9) {
      throw new RuntimeException("Sorting criteria is greater than 9")
    }
    var criteriaStr: String = "Cri:" + criteria.code.toString
    val cedict = generateCedictcomparison(inputText, textType)
    val charset = generateCharsetcomparison(inputText, textType)
    val wordFreq = generateBcluAndSinicaCodes(inputText, textType)

    if (graphs.length == 1) {
      output = criteriaStr + "," + "CedictPrim:" + cedict._1 + "CharPrim:" + charset._1 + "WordPrim:" + wordFreq._1
        + "CedictSec:" + cedict._2 + "CharSec:"+ charset._2 + "WordSec:" + wordFreq._2 + inputText
    } else if (graphs.length > 1) {
      output = criteriaStr + "," + "CedictPrim:" + cedict._1 + "WordPrim:" + wordFreq._1 + "CharPrim:" + charset._1
        + "CedictSec:" + cedict._2 + "WordSec:" + wordFreq._2 + "CharSec:" + charset._2 + inputText


    } else {
      throw new RuntimeException("empty character string - cant be sorted")
    }
    return output
  }

  private def getFixedLengthFromList(input: List[Int], length: Int): String = {
    val fillWithNines = "9" * length

    val result = input.map { n =>
      if (n == Int.MaxValue) {
        fillWithNines
      } else {
        val tempString = n.toString
        "0".repeat(length - tempString.length) + tempString
      }
    }.mkString(",")
    return result
  }

  private def generateCedictcomparison(inputText: String,
                                       textType: AtextType): (String, String) = {
    var outputString: String = ""
    val cedictMap: AcedictColl = AsingletonsForTests.cedict
    val resultSimp = getCedictTrueOrFalseStr(inputText, cedictMap.simplifiedAllHanItems)
    val resultTrad = getCedictTrueOrFalseStr(inputText, cedictMap.traditionalAllHanItems)
    if (textType == AtextType.Simplified) {
      return (resultSimp, resultTrad)
    } else if (textType == AtextType.Traditional) {
      return (resultTrad, resultSimp)
    } else {
      throw new RuntimeException("unknown text type")
    }
  }

  private def getCedictTrueOrFalseStr(inputText: String, cedict: Set[AcedictEntry]): String = {
    if (cedict.contains(AcedictEntry(inputText))) {
      return "2"
    }
    val graphs = Grapheme.splitIntoGraphemes(inputText)
    val allExists: Set[Boolean] = graphs.map(x => cedict.contains(AcedictEntry(x))).toSet
    if (allExists.size == 1 && allExists.head == true) {
      return "2"
    } else {
      return "3"
    }
  }


  private def generateBcluAndSinicaCodes(inputText: String,
                                         textType: AtextType): (String, String) = {
    var outputString: String = ""
    val blcuHit: immutable.HashMap[String, Int] = AsingletonsForTests.blcuData
    val sinicaHit: immutable.HashMap[String, Int] = AsingletonsForTests.sinicaData
    val resultBLCU: Int = getBLCUandSINICANumbers(inputText, blcuHit)
    val resultSINICA: Int = getBLCUandSINICANumbers(inputText, sinicaHit)

    val strBCLU: String = getFixedLengthFromList(List(resultBLCU),7)
    val strSINICA: String = getFixedLengthFromList(List(resultSINICA),7)

    if (textType == AtextType.Simplified) {
      return (strBCLU, strSINICA)
    } else if (textType == AtextType.Traditional) {
      return (strSINICA, strBCLU)
    } else {
      throw new RuntimeException("unknown text type")
    }
  }

  private def getBLCUandSINICANumbers(inputText: String,
                                     charset: immutable.HashMap[String, Int]): Int = {
    val rawOptions: Option[Int] = charset.get(inputText)
    if (rawOptions.isDefined) {
      return rawOptions.get
    } else {
      return Int.MaxValue
    }
  }

  private def generateCharsetcomparison(inputText: String,
                                       textType: AtextType): (String, String) = {
    var outputString: String = ""
    val charsetJunda: immutable.HashMap[String, Int] = AsingletonsForTests.junda
    val charsetTzai: immutable.HashMap[String, Int] = AsingletonsForTests.tzai
    val resultJunda: List[Int] = getJundaAndTzaiNumbers(inputText, charsetJunda)
    val resultTzai: List[Int] = getJundaAndTzaiNumbers(inputText, charsetTzai)

    val strJunda: String = getFixedLengthFromList(resultJunda, 5)
    val strTzai: String = getFixedLengthFromList(resultTzai, 5)

    if (textType == AtextType.Simplified) {
      return (strJunda , strTzai)
    } else if (textType == AtextType.Traditional) {
      return (strTzai , strJunda)
    } else {
      throw new RuntimeException("unknown text type")
    }
  }

  private def getJundaAndTzaiNumbers(inputText: String,
                                     charset: immutable.HashMap[String, Int]): List[Int] = {
    val graphs: List[String] = Grapheme.splitIntoGraphemes(inputText)
    val rawOptions: List[Option[Int]] = graphs.map(x => charset.get(x))
    val sorted: List[Int] = rawOptions.map {
      case Some(value) => value
      case None => Int.MaxValue
    }.sorted
    return sorted
  }

  def lettercodeComparison(lettercodeThis: String, lettercodeThat: String): Int = {
    val lenThis = lettercodeThis.length
    val lenThat = lettercodeThat.length
    if (lenThis < lenThat) {
      return -1
    }
    else if (lenThis > lenThat) {
      return 1
    }
    else {
      val stringcomparison = lettercodeThis.compareTo(lettercodeThat)
      return stringcomparison
    }
  }

  def wordFreq(inputWord: List[String], freq: immutable.HashMap[String, Int]): Int = {
    val getVal = freq.get(inputWord.mkString(""))
    if (getVal.isEmpty) {
      return Int.MaxValue
    } else {
      return getVal.get
    }
  } 

  def charsetComparison(charset: List[Int]): List[Int] = {
    charset.sorted
  }

  def cedictComparison(cedict: List[Boolean]): Boolean = {
    !cedict.contains(false)
  }

  def hancharComparison(hanchars: List[String]): List[Int] = {
    hanchars.flatMap(_.codePoints().toArray).sorted.toList
  }
}