package CodeAnomalies

import GenerateOutput.{GenerateOutputStrings, ReadConfigFiles}
import OutputTranslation.OutputSorting
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

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
    val hansimp = OutputSorting.allSimplified
    val hantrad = OutputSorting.allTraditional

    nonhan.size shouldBe 80
    hantrad.size shouldBe 133369
    hansimp.size shouldBe 119546
    
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

    withinNineThreeCode_twoChar_string.size shouldBe 35759
    beyondNineThreeCode_twoChar_string.size shouldBe 27747

    withinNineFiveCode_twoChar_string.size shouldBe 59059
    beyondNineFiveCode_twoChar_string.size shouldBe 186

    withinNineFiveCode_MoreThanTwoChar_string.size shouldBe 48807
    beyondNineFiveCode_MoreThanTwoChar_string.size shouldBe 71

    //val allSimp: Map[String, List[OutputEntry]] = getAllSimplified()

    val test = ""

  }

}
