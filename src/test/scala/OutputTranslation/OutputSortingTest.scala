package OutputTranslation

import ElementGenerator.ElementTranslateToAlphabet
import UtilityClasses.CharSystem.Junda
import UtilityClasses.{CedictEntry, Grapheme, OutputEntry, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.JundaFrequency.JundaData
import staticFileGenerators.SpecialCharacters.ReadSpecialCharacters
import staticFileGenerators.TzaiFrequency.TzaiData
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.{SortedMap, mutable}

class OutputSortingTest extends AnyFlatSpec with Matchers {

  // create a test that tests if there are any z characters in cedict or conway
  val jundaAboveNine: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda.filter(x => x._2.size > 9)
  val tzaiAboveNine: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai.filter(x => x._2.size > 9)

  private def getTestStringJunda(code: String, judaFull: SortedMap[String, List[OutputEntry]] ): String = {
    val code_eongo: List[OutputEntry] = judaFull.get(code).get
    val str_eongo: String = code_eongo
      .map(x => x.chineseStr + " " +
        x.jundaReverseOrder.map(y =>
            " J: " + dataToStringJunda(y.junda) +
            " T: " + dataToStringTzai(y.tzai)).mkString(" ")
        + " " + x.meaning
        + " uni: " +  unicodeOrdinals(x.chineseStr)).mkString("\r\n").trim
    str_eongo
  }

  private def unicodeOrdinals(s: String): String = {
    val codePoints = s.codePoints().toArray
    val ordinals = codePoints.map(cp => f"U+${cp}%04X")

    ordinals.mkString("[", ", ", "]")
  }

  private def getTestStringTzai(code: String, full: SortedMap[String, List[OutputEntry]]): String = {
    val code_eongo: List[OutputEntry] = full.get(code).get
    val str_eongo: String = code_eongo
      .map(x => x.chineseStr + " " +
        x.tzaiReverseOrder.map(y =>
          " T: " + dataToStringTzai(y.tzai) +
            " J: " + dataToStringJunda(y.junda)).mkString(" ")
        + " " + x.meaning
        + " uni: " + unicodeOrdinals(x.chineseStr)).mkString("\r\n").trim
    str_eongo
  }


  it should "junda and tzai - test the sorting of single characters" in {

    val outJunda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val str_eongo: String = getTestStringJunda("brho", outJunda)
    val testJundaStr: String =
      """惧  J: 1616 T: None  uni: [U+60E7]
        |憬  J: 3693 T: 3048  uni: [U+61AC]
        |怏  J: 4369 T: 5512  uni: [U+600F]
        |愦  J: 5744 T: None  uni: [U+6126]
        |憫   uni: [U+61AB]
        |憒   uni: [U+6192]
        |懆   uni: [U+61C6]
        |惿   uni: [U+60FF]
        |惈   uni: [U+60C8]
        |愄   uni: [U+6104]
        |懪   uni: [U+61EA]
        |㤤   uni: [U+3924]
        |㥏   uni: [U+394F]
        |㥗   uni: [U+3957]
        |㦨   uni: [U+39A8]
        |怾   uni: [U+603E]
        |悞   uni: [U+609E]
        |悮   uni: [U+60AE]
        |愪   uni: [U+612A]
        |憹   uni: [U+61B9]""".stripMargin
    val resJunda: Boolean = str_eongo.trim == testJundaStr.trim

    val outTzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai
    val str_eongo_tzai: String = getTestStringTzai("brho", outTzai)
    val testTzaiStr: String =
      """憬  T: 3048 J: 3693  uni: [U+61AC]
        |憫  T: 3140 J: None  uni: [U+61AB]
        |怏  T: 5512 J: 4369  uni: [U+600F]
        |憒  T: 6121 J: None  uni: [U+6192]
        |懆  T: 6211 J: None  uni: [U+61C6]
        |惿  T: 8966 J: None  uni: [U+60FF]
        |惈  T: 10137 J: None  uni: [U+60C8]
        |愄  T: 11473 J: None  uni: [U+6104]
        |懪  T: 12685 J: None  uni: [U+61EA]
        |惧   uni: [U+60E7]
        |愦   uni: [U+6126]
        |㤤   uni: [U+3924]
        |㥏   uni: [U+394F]
        |㥗   uni: [U+3957]
        |㦨   uni: [U+39A8]
        |怾   uni: [U+603E]
        |悞   uni: [U+609E]
        |悮   uni: [U+60AE]
        |愪   uni: [U+612A]
        |憹   uni: [U+61B9]""".stripMargin
    val resTzai: Boolean = str_eongo_tzai.trim == testTzaiStr.trim

    resJunda shouldBe true
    resTzai shouldBe true

  }

  it should "junda and tzai - test the sorting of words" in {

    val outJunda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val str_eongo: String = getTestStringJunda("eongo", outJunda)
    val testJundaStr: String =
    """米果  J: 575 T: 1154  J: 165 T: 99 rice cracker uni: [U+7C73, U+679C]
      |美景  J: 814 T: 878  J: 151 T: 219 beautiful scenery uni: [U+7F8E, U+666F]
      |美味  J: 844 T: 709  J: 151 T: 219 delicious/delicious food/delicacy uni: [U+7F8E, U+5473]
      |火暴  J: 1028 T: 871  J: 433 T: 527 variant of 火爆[huo3 bao4] uni: [U+706B, U+66B4]
      |兼具  J: 1515 T: 1526  J: 391 T: 679 to combine/to have both uni: [U+517C, U+5177]
      |炊具  J: 3333 T: 3313  J: 391 T: 679 cooking utensils/cookware/cooker uni: [U+708A, U+5177]
      |炊爨  J: 6143 T: 9314  J: 3333 T: 3313 to light a fire and cook a meal uni: [U+708A, U+7228]
      |義縣  J: 9461 T: 1032  J: 6089 T: 326 Yi county in Jinzhou 錦州|锦州, Liaoning uni: [U+7FA9, U+7E23]
      |榮景  J: None T: 926  J: 814 T: 878 period of prosperity uni: [U+69AE, U+666F]
      |榮縣  J: None T: 926  J: 9461 T: 1032 Rong county in Zigong 自貢|自贡[Zi4 gong4], Sichuan uni: [U+69AE, U+7E23]""".stripMargin
    val resJunda: Boolean = str_eongo.trim == testJundaStr.trim

    val outTzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai
    val str_eongo_tzai: String = getTestStringTzai("eongo", outTzai)
    val testTzaiStr: String =
    """美味  T: 709 J: 844  T: 219 J: 151 delicious/delicious food/delicacy uni: [U+7F8E, U+5473]
      |火暴  T: 871 J: 1028  T: 527 J: 433 variant of 火爆[huo3 bao4] uni: [U+706B, U+66B4]
      |美景  T: 878 J: 814  T: 219 J: 151 beautiful scenery uni: [U+7F8E, U+666F]
      |榮景  T: 926 J: None  T: 878 J: 814 period of prosperity uni: [U+69AE, U+666F]
      |義縣  T: 1032 J: 9461  T: 326 J: 6089 Yi county in Jinzhou 錦州|锦州, Liaoning uni: [U+7FA9, U+7E23]
      |榮縣  T: 1032 J: 9461  T: 926 J: None Rong county in Zigong 自貢|自贡[Zi4 gong4], Sichuan uni: [U+69AE, U+7E23]
      |米果  T: 1154 J: 575  T: 99 J: 165 rice cracker uni: [U+7C73, U+679C]
      |兼具  T: 1526 J: 1515  T: 679 J: 391 to combine/to have both uni: [U+517C, U+5177]
      |炊具  T: 3313 J: 3333  T: 679 J: 391 cooking utensils/cookware/cooker uni: [U+708A, U+5177]
      |炊爨  T: 9314 J: 6143  T: 3313 J: 3333 to light a fire and cook a meal uni: [U+708A, U+7228]""".stripMargin
    val resTzai: Boolean = str_eongo_tzai.trim == testTzaiStr.trim

    resJunda shouldBe true
    resTzai shouldBe true

  }

  private def dataToStringJunda(input: Option[JundaData]): String = {
    if (input.isDefined) {
      return input.get.ordinal.toString
    } else {
      return "None"
    }
  }

  private def dataToStringTzai(input: Option[TzaiData]): String = {
    if (input.isDefined) {
      return input.get.ordinal.toString
    } else {
      return "None"
    }
  }

  it should "test punctuation" in {

    val outJunda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val outTzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai

    val codesStartingWithZ: String = outJunda
      .filter(x => x._1.startsWith("z")).map(y => y._1 + " " + y._2.map(z => z.chineseStr).mkString(" ").trim).mkString("\n").trim

    codesStartingWithZ ==
    """z % ○
      |zc 3C
      |zng π日
      |zp 3P
      |zq 3Q
      |zz 88
      |zzgnu 21三體綜合症
      |zzgum 21三体综合症
      |zzih 95后
      |zzil 95後
      |zzz 421 996
      |zzzpi 502膠
      |zzzpo 502胶
      |zzzzq 2019冠状病毒病 2019冠狀病毒病""".stripMargin

  }

  it should "find any cedict or junda characters with z codes" in {
    val conwayOutFull: Set[OutputEntry] = OutputSorting.conFull
    //val cedictMap: Map[String, CedictEntry] = GenerateCedictMap.cedictMap
    val cedictOut: Set[OutputEntry] = OutputSorting.cedictSetOut
    val conwayMap: Map[Grapheme, StaticFileCharInfoWithLetterConway] = ElementTranslateToAlphabet.completeTranslatedConwayMap
    val lettersExceptZ = lettersSansAscii()

    val otuputStrWithLies: StringBuilder = new StringBuilder
    var overlap: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    var overlapChars: mutable.Set[Char] = mutable.Set[Char]()
    for (entry <- conwayOutFull) {
      for (eachCode <- entry.codes) {
        for (eachLetter <- eachCode) {
          if (!lettersExceptZ.contains(eachLetter) && eachCode.size > 1) {
            overlap.add(entry)
            overlapChars.add(eachLetter)
          }
        }
      }
    }
    for (entry <- cedictOut) {
      for (eachCode <- entry.codes) {
        for (eachLetter <- eachCode) {
          if (!lettersExceptZ.contains(eachLetter) && eachCode.size > 1) {
            val allGraphemes: Set[Grapheme] = Grapheme.splitIntoGraphemes(entry.chineseStr).map(y => Grapheme(y)).toSet
            val graphemesMissingFromConway: Set[Grapheme] = allGraphemes.filter(x => !conwayMap.contains(x)).toSet
            overlap.add(entry)
            overlapChars.add(eachLetter)
          }
        }
      }
      val wordGraphemes: Set[Grapheme] = Grapheme.splitIntoGraphemes(entry.chineseStr).map(y => Grapheme(y)).toSet
      val wordGrapehmesMissing: Set[Grapheme] = wordGraphemes.filter(x => !conwayMap.contains(x)).toSet
      if (wordGrapehmesMissing.size > 0) {
        val chnStr: String = entry.chineseStr
        val codesToShow: String = entry.codes.mkString(" ")
        otuputStrWithLies.append(chnStr).append(" ").append(codesToShow).append("\n")
        val test = ""
      }
    }
    overlapChars.size shouldBe 1
    overlapChars.toList(0) shouldBe 'z'
    val allGraphemes: Set[Grapheme] = overlap.map(x => Grapheme.splitIntoGraphemes(x.chineseStr).map(y => Grapheme(y))).flatten.toSet
    val graphemesMissingFromConway: Set[Grapheme] = allGraphemes.filter(x => !conwayMap.contains(x)).toSet

    overlap.size shouldBe 364 // there are many lines that contain z codes
  }

  private def lettersSansAscii(): Set[Char] = {
    val letters = ('a' to 'y').toSet
    return letters
  }

  it should "check the codes of cedict and conway" in {
    val conwayOutFull: Set[OutputEntry] = OutputSorting.conFull
    val cedictOut: Set[OutputEntry] = OutputSorting.cedictSetOut
    val conwayMap: Map[Grapheme, StaticFileCharInfoWithLetterConway] = ElementTranslateToAlphabet.completeTranslatedConwayMap


    var overlap: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    var desiredChar1: Option[OutputEntry] = None
    var desiredChar2: Option[OutputEntry] = None
    for (entry <- conwayOutFull) {
      if (entry.chineseStr == "臒") {
        desiredChar1 = Some(entry)
      }
    }
    desiredChar1 should not be None
    desiredChar1.get.codes shouldBe Set("ptfful", "pgfxul", "ptxful", "pgfful", "pgfl", "pgxl",
      "pgxful", "ptfvbl", "ptfl", "ptxl", "pgfvbl", "ptfxul")

    val test: String = ""
  }

}
