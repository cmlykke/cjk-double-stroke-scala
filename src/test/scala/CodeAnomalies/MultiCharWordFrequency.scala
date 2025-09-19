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
