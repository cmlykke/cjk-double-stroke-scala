package CodeAnomalies

import GenerateOutput.{GenerateOutputStrings, ReadConfigFiles}
import OutputTranslation.OutputSorting
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.util.chaining.scalaUtilChainingOps
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success}
import scala.math.Ordering.Implicits.*
import scala.jdk.StreamConverters.*

class MultiCharWordFrequency extends AnyFlatSpec with Matchers {

  private val generatecedict = new GenerateCedictMap()
  val generate = new GenerateOutputStrings()
  val cedictTupple = generatecedict.generateCedictList()
  val simpCedictSet: Set[String] = cedictTupple._3.view.map(_.chineseStr).toSet
  val linemapSimp: Map[String, List[OutputEntry]] = GenerateOutputStrings.mapFullJunda
  val conwayMap: Set[OutputEntry] = OutputSorting.conFull

  private def convertCedictCodeMapToStrMap(): Map[String, (OutputEntry, List[String])] = {
    linemapSimp.view
      .map { case (key, value) =>
        (key, value.filter(x => simpCedictSet.contains(x.chineseStr) && !x.chineseStr.forall(_.toInt <= 127)))
      }
      .flatMap { case (key, entries) => entries.map(entry => (entry, key)) }
      .toList
      .groupBy(_._1.chineseStr)
      .map { case (chineseStr, tuples) =>
        (chineseStr, (tuples.head._1, tuples.map(_._2).sorted))
      }
      .toMap
  }

  private def getBeyondNineSimplifiedCode(codelength: Int): Map[String, (OutputEntry, List[String])] = {
    linemapSimp.view
      .filter { case (key, value) => key.size == codelength && value.size > 9 }
      .map { case (key, value) => (key, value.drop(9)) }
      .map { case (key, value) =>
        (key, value.filter(x => simpCedictSet.contains(x.chineseStr) && !x.chineseStr.forall(_.toInt <= 127)))
      }
      .flatMap { case (key, entries) => entries.map(entry => (entry, key)) }
      .toList
      .groupBy(_._1.chineseStr)
      .map { case (chineseStr, tuples) =>
        (chineseStr, (tuples.head._1, tuples.map(_._2).sorted))
      }
      .toMap
  }

  private def getWithinNineSimplifiedCode(codelength: Int): Map[String, (OutputEntry, List[String])] = {
    linemapSimp.view
      .filter { case (key, value) => key.size == codelength}
      .map { case (key, value) => (key, value.take(9)) }
      .map { case (key, value) =>
        (key, value.filter(x => simpCedictSet.contains(x.chineseStr) && !x.chineseStr.forall(_.toInt <= 127)))
      }
      .flatMap { case (key, entries) => entries.map(entry => (entry, key)) }
      .toList
      .groupBy(_._1.chineseStr)
      .map { case (chineseStr, tuples) =>
        (chineseStr, (tuples.head._1, tuples.map(_._2).sorted))
      }
      .toMap
  }


  private def getSimplifiedTuple(beyondNine: Map[String, (OutputEntry, List[String])]): List[(String, OutputEntry, List[String])] = {
    beyondNine.toList.map { case (key, (entry, strings)) => (key, entry, strings) }
      .sortBy { case (_, entry, _) => (entry.BCLUord, entry.jundaReverseOrder) }(
        Ordering.Tuple2(Ordering[Int], Ordering[List[Int]])
      )
  }

  private def stringFromTuple(tupleTest: List[(String, OutputEntry, List[String])]): List[String] = {
    tupleTest.map { x =>
      x._1 +
        " codes: " + x._3.mkString(" ") +
        " BLCU word freq: " + (if (x._2.BCLUord > 10_000_000) "None" else x._2.BCLUord.toString) +
        " junda freq: " + x._2.jundaReverseOrderG.map(_.junda.map(_.ordinal.toString).getOrElse("None")).mkString(" ") +
        " pinyin: " + x._2.pron +
        " meaning: " + x._2.meaning
    }
  }

  it should "test that pgh (vt) codes will yield words in the correct order" in {
    //NOTE: the data needs to be investigated. 周末 does not apear by itself in the BCLU dataset!!!
    val pgh = linemapSimp.get("pgh")

    val pgh_formed: List[String] = pgh.get.map(x => x.chineseStr + " " + x.BCLUord + " " + x.meaning)

    val pgh_compare_raw: String =
      """周一 4031 Monday
        |周三 5693 Wednesday
        |周二 5859 Tuesday
        |周刊 7604 weekly publication/weekly
        |名次 12453 position in a ranking of names/place/rank
        |乌云 19348 black cloud
        |周瑜 19755 Zhou Yu (175-210), famous general of the southern Wu kingdom and victor of the battle of Redcliff/in Romance of the Three Kingdoms 三國演義|三国演义[San1 guo2 Yan3 yi4], absolutely no match for Zhuge Liang 諸葛亮|诸葛亮[Zhu1 ge3 Liang4]
        |周天 26824 Sunday
        |句式 31217 sentence pattern/sentence structure/syntax
        |句型 36753 sentence pattern (in grammar)
        |周璇 41775 Zhou Xuan (1918-1957), Chinese singer and film actress
        |胎动 47816 fetal movement
        |周礼 56029 the Rites of Zhou (in Confucianism)
        |乌青 63459 bluish black/bruise; bruising (CL:塊|块[kuai4])
        |名表 2147483647 famous watch (i.e. expensive brand of wristwatch)
        |名素 2147483647 variant of 名宿[ming2 su4]
        |周末 2147483647 weekend
        |鸟击 2147483647 bird strike (aviation)
        |周禮 2147483647 the Rites of Zhou (in Confucianism)
        |問責 2147483647 to hold accountable; to blame; to censure; to apportion blame
        |刍 40839 to mow or cut grass/hay/straw/fodder""".stripMargin.trim

    val pgh_compare: List[String] = pgh_compare_raw.split("\\R").toList

    pgh_formed shouldBe pgh_compare
  }

  it should "test the total number of single characters and words" in {
    val jundaRaw = OutputSorting.mapFullJunda
    val tzaiRaw = OutputSorting.mapFullTzai

    val allStrings: Set[String] = jundaRaw.values.flatten
      .map(outputEntry => outputEntry.chineseStr)
      .filter(text => text.codePoints().toScala(List).exists(_ >= 0x2E80)).toSet
    
    val singles: List[String] = allStrings.filter(text => text.codePoints().toArray.length == 1).toList.sorted
    val words: List[String] = allStrings.filter(text => text.codePoints().toArray.length > 1).toList.sorted

    singles.size shouldBe 28312
    words.size shouldBe 179752
    
    jundaRaw.size shouldBe tzaiRaw.size
  }
  
  it should "10058 entries in conway are missing from cedict" in {
    val nonhan = OutputSorting.allNonHan
    val hansimp: Set[String] = OutputSorting.allSimplified
    val hantrad: Set[String] = OutputSorting.allTraditional

    nonhan.size shouldBe 80
    hantrad.size shouldBe 133369
    hansimp.size shouldBe 119546

    //get junda, tzai, and cedict
    val junda_prelim_set: Set[String] = OutputSorting.jundaMap.map(x => x._1).toSet
    val tzai_set: Set[String] = OutputSorting.tzaiMap.map(x => x._1).toSet
    val conway_prelim_set: Set[String] = OutputSorting.conwaySet.map(x => x.char)

    val cedict_simp_set: Set[String] = OutputSorting.cedictSet
      .filter(x => x.system.equals(CharSystem.Junda))
      .map(x => x.chineseStr)
      .toSet
      .pipe(createStringOfAllWordsAndCharacters)

    val cedict_trad_set: Set[String] = OutputSorting.cedictSet
      .filter(x => x.system.equals(CharSystem.Tzai))
      .map(x => x.chineseStr)
      .toSet
      .pipe(createStringOfAllWordsAndCharacters)

    val cedict_other_set: Set[String] = OutputSorting.cedictSet
      .filter(x => x.system.equals(CharSystem.NotHanChar))
      .map(x => x.chineseStr)
      .toSet
      .pipe(createStringOfAllWordsAndCharacters)

    val cedict_allsingle_set: Set[String] = OutputSorting.cedictSet
      .map(x => x.chineseStr)
      .toSet
      .pipe(createStringOfAllWordsAndCharacters)
      .filter(x => x.codePoints().count() == 1)

    val allsingles_nonConway: Set[String] = cedict_allsingle_set ++ tzai_set ++ junda_prelim_set

    //get true simplified and traditional characters from junda_prelim_set
    val junda_simp_set: Set[String] = junda_prelim_set.filter(x => cedict_simp_set.contains(x))
    val junda_nonSimp_set: Set[String] = junda_prelim_set.filter(x => !cedict_simp_set.contains(x))
    val junda_nonTradNonSimp_set: Set[String] = junda_nonSimp_set.filter(x => !cedict_trad_set.contains(x))
    //some characters in the junda set is not found in the cedict dictionary. it is assumed that they will be traditional
    junda_nonTradNonSimp_set.size shouldBe 1032

    //get true simplified and traditional characters from conway_prelim_set
    val conway_simp_set: Set[String] = conway_prelim_set.filter(x => cedict_simp_set.contains(x))
    val conway_trad_set: Set[String] = conway_prelim_set.filter(x => cedict_trad_set.contains(x))
    val conway_nonTradNonSimp_set: Set[String] = conway_prelim_set.filter(x => !cedict_trad_set.contains(x) && !cedict_simp_set.contains(x))
    //some characters in the conway set is not found in the cedict dictionary. it is assumed that they will be traditional
    conway_nonTradNonSimp_set.size shouldBe 13707

    //see if any characters in junda, tzai and cedict are missing from conway
    val singlesMissingFromConway: Set[String] = allsingles_nonConway.filter(x => !conway_prelim_set.contains(x))
    val singlesMissingFromConway_codepoints = singlesMissingFromConway
      .map(_.codePoints().toArray).toList.flatten.sorted.reverse
    singlesMissingFromConway_codepoints.size shouldBe 52
    singlesMissingFromConway_codepoints(0) shouldBe 9675 // the first codepoint is "White circle" ie. not a character
    singlesMissingFromConway_codepoints(1) shouldBe 960 // the next codepoint is pi (π) or lower
    val conwayMissingFromsingles: Set[String] = conway_prelim_set.filter(x => !allsingles_nonConway.contains(x))
    conwayMissingFromsingles.size shouldBe 10022

    //create simplified and traditional sets from junda, tzai, cedict and conway
    val allSimplified: Set[String] = cedict_simp_set
    val presumedTrad: Set[String] = junda_nonSimp_set ++ conway_nonTradNonSimp_set
    val allTraditional: Set[String] = tzai_set ++  cedict_trad_set ++ presumedTrad

    val simplifiedmissing: Set[String] = hansimp.filter(x => !allSimplified.contains(x))
    val simplifiedtoomany: Set[String] = allSimplified.filter(x => !hansimp.contains(x))
    val traditionalmissing: Set[String] = hantrad.filter(x => !allTraditional.contains(x))
    val traditionaltoomany: Set[String] = allTraditional.filter(x => !hantrad.contains(x))


    // *******************************************************

    val codedTzai: Set[String] = OutputSorting.mapFullTzai.values.flatten.map(x => x.chineseStr).toSet
    val codedjunda: Set[String] = OutputSorting.mapFullJunda.values.flatten.map(x => x.chineseStr).toSet
    
    val missingJunda: Set[String] = codedjunda.filter(x => !hansimp.contains(x) && !hantrad.contains(x))
    val missingTzai: Set[String] = codedTzai.filter(x => !hansimp.contains(x) && !hantrad.contains(x))
    
    missingJunda.size == 10058
    missingTzai.size == 10058

    //all conway should be in trad and simp
    val conwayStr: Set[String] = OutputSorting.conwaySet.map(x => x.char).toSet
    val combinedSimpAndTrad: Set[String] = hansimp ++ hantrad
    val conwayMissingFromCombined: Set[String] = conwayStr.filter(x => !combinedSimpAndTrad.contains(x))

    conwayMissingFromCombined.size shouldBe 0
  }
  
  
  it should "test output junda file" in {
    /// **************************************** new code

    val beyondNineFiveCode: Map[String, (OutputEntry, List[String])] = getBeyondNineSimplifiedCode(5)
    val beyondNineFiveCode_twoChar: Map[String, (OutputEntry, List[String])] = beyondNineFiveCode.filter {
      case (key, _) => key.codePoints().count() == 2
    }
    val tupple_one: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(beyondNineFiveCode_twoChar)

    val beyondNineFiveCode_MoreThanTwoChar: Map[String, (OutputEntry, List[String])] = beyondNineFiveCode.filter {
      case (key, _) => key.codePoints().count() > 2
    }
    val tupple_two: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(beyondNineFiveCode_MoreThanTwoChar)

    val beyondNineThreeCode: Map[String, (OutputEntry, List[String])] = getBeyondNineSimplifiedCode(3)
    val beyondNineThreeCode_twoChar: Map[String, (OutputEntry, List[String])] = beyondNineThreeCode.filter {
      case (key, _) => key.codePoints().count() == 2
    }
    val tupple_three: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(beyondNineThreeCode_twoChar)

    val withinNineFiveCode: Map[String, (OutputEntry, List[String])] = getWithinNineSimplifiedCode(5)
    val withinNineFiveCode_twoChar: Map[String, (OutputEntry, List[String])] = withinNineFiveCode.filter {
      case (key, _) => key.codePoints().count() == 2
    }
    val tupple_four: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(withinNineFiveCode_twoChar)

    val withinNineFiveCode_MoreThanTwoChar: Map[String, (OutputEntry, List[String])] = withinNineFiveCode.filter {
      case (key, _) => key.codePoints().count() > 2
    }
    val tupple_five: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(withinNineFiveCode_MoreThanTwoChar)

    val withinNineThreeCode: Map[String, (OutputEntry, List[String])] = getWithinNineSimplifiedCode(3)
    val withinNineThreeCode_twoChar: Map[String, (OutputEntry, List[String])] = withinNineThreeCode.filter {
      case (key, _) => key.codePoints().count() == 2
    }
    val tupple_six: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(withinNineThreeCode_twoChar)


    val withinNineThreeCode_twoChar_string: List[String] = stringFromTuple(tupple_six)
    val beyondNineThreeCode_twoChar_string: List[String] = stringFromTuple(tupple_three)

    val withinNineFiveCode_twoChar_string: List[String] = stringFromTuple(tupple_four)
    val beyondNineFiveCode_twoChar_string: List[String] = stringFromTuple(tupple_one)

    val withinNineFiveCode_MoreThanTwoChar_string: List[String] = stringFromTuple(tupple_five)
    val beyondNineFiveCode_MoreThanTwoChar_string: List[String] = stringFromTuple(tupple_two)

    withinNineThreeCode_twoChar_string.size shouldBe 35928
    beyondNineThreeCode_twoChar_string.size shouldBe 27561

    withinNineFiveCode_twoChar_string.size shouldBe 59076
    beyondNineFiveCode_twoChar_string.size shouldBe 134

    withinNineFiveCode_MoreThanTwoChar_string.size shouldBe 48805
    beyondNineFiveCode_MoreThanTwoChar_string.size shouldBe 78

    // test why the two word tests doesnt add up to the same number
    // (59076 + 134  is not equal to 35928 + 27561)
    // mapFullJunda


    //val allSimp: Map[String, List[OutputEntry]] = getAllSimplified()


    val test = ""

  }

  private def createStringOfAllWordsAndCharacters(cedict: Set[String]): Set[String] = {
    val singles: Set[String] = cedict.flatMap(_.codePoints().mapToObj(cp => new String(Character.toChars(cp))).toScala(Seq)).toSet
    return cedict ++ singles
  }

}
