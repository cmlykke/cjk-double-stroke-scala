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

  private def unicodeOrdinals(s: String): String = {
    val codePoints = s.codePoints().toArray
    val ordinals = codePoints.map(cp => f"U+${cp}%04X")

    ordinals.mkString("[", ", ", "]")
  }
  
  private def getTestStringJunda(code: String, judaFull: SortedMap[String, List[OutputEntry]]): String = {
    if (code == "crho") {
      val test = ""
    }
    val code_eongo: List[OutputEntry] = judaFull.get(code).getOrElse(List())
    val sb = new StringBuilder
    for (entry <- code_eongo) {
      if (entry.chineseStr == "惿") {
        val test2 = ""
      }
      sb.append(entry.chineseStr).append(" ")
      sb.append("J: ")
      for (reverseOrder <- entry.jundaReverseOrderG) {
        sb.append(dataToStringJunda(reverseOrder.junda)).append(" ")
      }
      sb.append("T: ")
      for (reverseOrder <- entry.tzaiReverseOrderG) {
        sb.append(dataToStringTzai(reverseOrder.tzai)).append(" ")
      }
      //sb.append("T: ").append(dataToStringTzai(reverseOrder.tzai)).append(" ")
      sb.append(entry.meaning).append(" ")
      sb.append("uni: ").append(unicodeOrdinals(entry.chineseStr)).append("\r\n")
    }
    // Trim the trailing new line and return the result
    sb.toString().trim
  }
  

  private def getTestStringTzai(code: String, full: SortedMap[String, List[OutputEntry]]): String = {
    val code_eongo: List[OutputEntry] = full.get(code).getOrElse(List())
    val sb = new StringBuilder
    for (entry <- code_eongo) {
      sb.append(entry.chineseStr).append(" ")
      sb.append("J: ")
      for (reverseOrder <- entry.jundaReverseOrderG) {
        sb.append(dataToStringJunda(reverseOrder.junda)).append(" ")
      }
      sb.append("T: ")
      for (reverseOrder <- entry.tzaiReverseOrderG) {
        sb.append(dataToStringTzai(reverseOrder.tzai)).append(" ")
      }
      sb.append(entry.meaning).append(" ")
      sb.append("uni: ").append(unicodeOrdinals(entry.chineseStr)).append("\r\n")
    }
    // Trim the trailing new line and return the result
    sb.toString().trim
  }
  
  it should "test the total number of single characters and multi-character words" in {
    val conwaySet: Set[OutputEntry] = OutputSorting.conFull
    val cedictSet: Set[OutputEntry] = OutputSorting.cedictSetOut
    val totalSet: Set[OutputEntry] = cedictSet union conwaySet
    
    val singleChars: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    val multiChars: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (each <- totalSet) {
      if (each.tzaiReverseOrderG.size > 1 && each.jundaReverseOrderG.size > 1) {
        multiChars.add(each)
      } else if (each.tzaiReverseOrderG.size == 1 && each.jundaReverseOrderG.size == 1) {
        singleChars.add(each)
      } else {
        throw new Exception("cant determine word length")
      }
    }
    
    singleChars.size shouldBe 28874
    multiChars.size shouldBe 179226    
  }
  
  it should "junda and tzai - test the sorting of single characters" in {

    val outJunda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    
    val firstChar = outJunda.filter(x => x._2.filter(y => y.chineseStr == "撒村").nonEmpty).toSet
    //lodjt
    val firstChar2 = outJunda.filter(x => x._2.filter(y => y.chineseStr == "掛繩").nonEmpty).toSet
    //lcuxh lcu
    
    // wbgo  rrgo  crgo
    val str_eongo: String = getTestStringJunda("crgo", outJunda)
    val testJundaStr: String =
      """惧 J: 1616 T: None to fear uni: [U+60E7]
        |憬 J: 3693 T: 3048 awaken uni: [U+61AC]
        |怏 J: 4369 T: 5512 discontented uni: [U+600F]
        |愦 J: 5744 T: None confused/troubled uni: [U+6126]
        |憫 J: None T: 3140 to sympathize; to pity; to feel compassion for/(literary) to feel sorrow; to be grieved uni: [U+61AB]
        |憒 J: None T: 6121 confused/troubled uni: [U+6192]
        |懆 J: None T: 6211 anxious/sad uni: [U+61C6]
        |惿 J: None T: 8966  uni: [U+60FF]
        |惈 J: None T: 10137 courageous/resolute and daring uni: [U+60C8]
        |愄 J: None T: 11473  uni: [U+6104]
        |懪 J: None T: 12685  uni: [U+61EA]
        |愪 J: None T: None  uni: [U+612A]
        |悮 J: None T: None to impede/to delay/variant of 誤|误[wu4] uni: [U+60AE]
        |憹 J: None T: None used in 懊憹|懊𢙐[ao4nao2] uni: [U+61B9]
        |悞 J: None T: None to impede/to delay/variant of 誤|误[wu4] uni: [U+609E]
        |怾 J: None T: None  uni: [U+603E]
        |㦨 J: None T: None  uni: [U+39A8]
        |㥗 J: None T: None  uni: [U+3957]
        |㥏 J: None T: None ashamed uni: [U+394F]
        |㤤 J: None T: None  uni: [U+3924]""".stripMargin
    val resJunda: Boolean = str_eongo.replaceAll("\\s", "") == testJundaStr.replaceAll("\\s", "")

    val outTzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai
    val str_eongo_tzai: String = getTestStringTzai("crgo", outTzai)
    val testTzaiStr: String =
      """憬 J: 3693 T: 3048 awaken uni: [U+61AC]
        |憫 J: None T: 3140 to sympathize; to pity; to feel compassion for/(literary) to feel sorrow; to be grieved uni: [U+61AB]
        |怏 J: 4369 T: 5512 discontented uni: [U+600F]
        |憒 J: None T: 6121 confused/troubled uni: [U+6192]
        |懆 J: None T: 6211 anxious/sad uni: [U+61C6]
        |惿 J: None T: 8966  uni: [U+60FF]
        |惈 J: None T: 10137 courageous/resolute and daring uni: [U+60C8]
        |愄 J: None T: 11473  uni: [U+6104]
        |懪 J: None T: 12685  uni: [U+61EA]
        |惧 J: 1616 T: None to fear uni: [U+60E7]
        |愦 J: 5744 T: None confused/troubled uni: [U+6126]
        |愪 J: None T: None  uni: [U+612A]
        |悮 J: None T: None to impede/to delay/variant of 誤|误[wu4] uni: [U+60AE]
        |憹 J: None T: None used in 懊憹|懊𢙐[ao4nao2] uni: [U+61B9]
        |悞 J: None T: None to impede/to delay/variant of 誤|误[wu4] uni: [U+609E]
        |怾 J: None T: None  uni: [U+603E]
        |㦨 J: None T: None  uni: [U+39A8]
        |㥗 J: None T: None  uni: [U+3957]
        |㥏 J: None T: None ashamed uni: [U+394F]
        |㤤 J: None T: None  uni: [U+3924]""".stripMargin
    val resTzai: Boolean = str_eongo_tzai.replaceAll("\\s", "") == testTzaiStr.replaceAll("\\s", "")

    resJunda shouldBe true
    resTzai shouldBe true

  }

  it should "junda and tzai - test the sorting of words" in {

    val outJunda: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullJunda
    val str_eongo: String = getTestStringJunda("eoxho", outJunda)
    val testJundaStr: String =
    """美味 J: 844 151 T: 709 219 delicious/delicious food/delicacy uni: [U+7F8E, U+5473]
      |美景 J: 814 151 T: 878 219 beautiful scenery uni: [U+7F8E, U+666F]
      |火暴 J: 1028 433 T: 871 527 variant of 火爆[huo3 bao4] uni: [U+706B, U+66B4]
      |炊具 J: 3333 391 T: 3313 679 cooking utensils/cookware/cooker uni: [U+708A, U+5177]
      |米果 J: 575 165 T: 1154 99 rice cracker uni: [U+7C73, U+679C]
      |兼具 J: 1515 391 T: 1526 679 to combine/to have both uni: [U+517C, U+5177]
      |炊爨 J: 6143 3333 T: 9314 3313 to light a fire and cook a meal uni: [U+708A, U+7228]
      |義縣 J: 9461 6089 T: 1032 326 Yi county in Jinzhou 錦州|锦州, Liaoning uni: [U+7FA9, U+7E23]
      |榮景 J: None 814 T: 926 878 period of prosperity uni: [U+69AE, U+666F]
      |榮縣 J: None 9461 T: 1032 926 Rong county in Zigong 自貢|自贡[Zi4 gong4], Sichuan uni: [U+69AE, U+7E23]""".stripMargin
    val resJunda: Boolean = str_eongo.replaceAll("\\s", "") == testJundaStr.replaceAll("\\s", "")

    val outTzai: SortedMap[String, List[OutputEntry]] = OutputSorting.mapFullTzai
    val str_eongo_tzai: String = getTestStringTzai("eoxho", outTzai)
    val testTzaiStr: String =
    """美景 J: 814 151 T: 878 219 beautiful scenery uni: [U+7F8E, U+666F]
      |兼具 J: 1515 391 T: 1526 679 to combine/to have both uni: [U+517C, U+5177]
      |美味 J: 844 151 T: 709 219 delicious/delicious food/delicacy uni: [U+7F8E, U+5473]
      |火暴 J: 1028 433 T: 871 527 variant of 火爆[huo3 bao4] uni: [U+706B, U+66B4]
      |榮景 J: None 814 T: 926 878 period of prosperity uni: [U+69AE, U+666F]
      |義縣 J: 9461 6089 T: 1032 326 Yi county in Jinzhou 錦州|锦州, Liaoning uni: [U+7FA9, U+7E23]
      |榮縣 J: None 9461 T: 1032 926 Rong county in Zigong 自貢|自贡[Zi4 gong4], Sichuan uni: [U+69AE, U+7E23]
      |米果 J: 575 165 T: 1154 99 rice cracker uni: [U+7C73, U+679C]
      |炊具 J: 3333 391 T: 3313 679 cooking utensils/cookware/cooker uni: [U+708A, U+5177]
      |炊爨 J: 6143 3333 T: 9314 3313 to light a fire and cook a meal uni: [U+708A, U+7228]""".stripMargin
    val resTzai: Boolean = str_eongo_tzai.replaceAll("\\s", "") == testTzaiStr.replaceAll("\\s", "")

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

    overlap.size shouldBe 25152 //19241 //22025 //15613 //364 // there are many lines that contain z codes
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
    desiredChar1.get.codes shouldBe Set(
      "ptns", "ptjs", "ptnjus", "ptjvcs", "ptjjus", "ptjnus",
      "phns", "phjs", "phnjus", "phjvcs", "phjjus", "phjnus" )

    val test: String = ""
  }

}
