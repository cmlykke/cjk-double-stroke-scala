package CodeAnomalies

import GenerateOutput.{GenerateOutputStrings, ReadConfigFiles}
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.util.{Failure, Success}
import scala.math.Ordering.Implicits.*

class MultiCharWordFrequency extends AnyFlatSpec with Matchers {

  private val generatecedict = new GenerateCedictMap()
  val generate = new GenerateOutputStrings()
  val cedictTupple = generatecedict.generateCedictList()
  val simpCedictSet: Set[String] = cedictTupple._3.view.map(_.chineseStr).toSet
  val linemapSimp: Map[String, List[OutputEntry]] = GenerateOutputStrings.mapFullJunda

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

  it should "test output junda file" in {

    val resultMapFiveCodeWords: Map[String, (OutputEntry, List[String])] = getBeyondNineSimplifiedCode(5)
    val sortedTupleListFiveCodeWords: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(resultMapFiveCodeWords)
    val fiveCodeWordsStringList: List[String] = stringFromTuple(sortedTupleListFiveCodeWords)
    fiveCodeWordsStringList.length shouldBe 257

    val threeLetterSimplifiedWithinNine: Map[String, (OutputEntry, List[String])] = getWithinNineSimplifiedCode(3).filter {
      case (_, eachval) => {
        eachval._1.chineseStr.codePoints().count() > 1
      }
    }
    //show strings with threeLetterWithinNine
    val threeLetterWithin_1: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(threeLetterSimplifiedWithinNine)

    //********** 35759 words that does show up among the first nine when written with three codes
    val threeLetterSimplifiedWithinNineList: List[String] = stringFromTuple(threeLetterWithin_1)
    threeLetterSimplifiedWithinNineList.size shouldBe 35759


    val fiveLetterSimplifiedWithinNine: Map[String, (OutputEntry, List[String])] = getWithinNineSimplifiedCode(5).filter {
      case (_, eachval) => {
        eachval._1.chineseStr.codePoints().count() > 2
      }
    }
    val fiveLetterWithin_1: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(fiveLetterSimplifiedWithinNine)
    //*********** words with more than two characters that always show up
    val fiveLetterSimplifiedWithinNineList: List[String] = stringFromTuple(fiveLetterWithin_1)
    fiveLetterSimplifiedWithinNineList.size shouldBe 48807

    //look at each key in resultMapFiveCodeWords, and remove the entries that are in threeLetterSimplifiedWithinNine
    val removedMissingFiveCodesThatExistsInThreeCodes: Map[String, (OutputEntry, List[String])] =
      resultMapFiveCodeWords.filter { case (key, value) =>
        threeLetterSimplifiedWithinNine.get(key).forall { case (_, threeLetterCodes) =>
          !value._2.map(_.take(3)).forall(threeLetterCodes.toSet.contains)
        }
      }

    val keptFiveCodesMatchingThreeCodes: Map[String, (OutputEntry, List[String])] =
      resultMapFiveCodeWords.filter { case (key, value) =>
        threeLetterSimplifiedWithinNine.get(key).exists { case (_, threeLetterCodes) =>
          value._2.map(_.take(3)).forall(threeLetterCodes.toSet.contains)
        }
      }
    resultMapFiveCodeWords.size shouldBe 257
    removedMissingFiveCodesThatExistsInThreeCodes.size + keptFiveCodesMatchingThreeCodes.size shouldBe 257
    val wordslostinFiveCodes: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(keptFiveCodesMatchingThreeCodes)

    //********* 7 two-character words that doesnt show up among the 9,
    //when written with 3 letters, but does show up when written with 5
    val wordslostinFiveCodesList: List[String] = stringFromTuple(wordslostinFiveCodes)
    wordslostinFiveCodesList.size shouldBe 7


    val missingThreeAndFive : List[(String, OutputEntry, List[String])] = getSimplifiedTuple(removedMissingFiveCodesThatExistsInThreeCodes)
    val missingThreeAndFiveTwoCharacter: List[(String, OutputEntry, List[String])] = missingThreeAndFive.filter {
      case (item) => item._1.codePoints().count() == 2
    }
    val missingThreeAndFiveThreePlusCharacter: List[(String, OutputEntry, List[String])] = missingThreeAndFive.filter {
      case (item) => item._1.codePoints().count() > 2
    }

    //******** words that are always missing, that have two characters
    val missingThreeAndFiveTwoCharacterList: List[String] = stringFromTuple(missingThreeAndFiveTwoCharacter)
    missingThreeAndFiveTwoCharacterList.size shouldBe 179

    //******** words that are always missing, that have more than two characters
    val missingThreeAndFiveThreePlusCharacterList: List[String] = stringFromTuple(missingThreeAndFiveThreePlusCharacter)
    missingThreeAndFiveThreePlusCharacterList.size shouldBe 71

    //************** words that are always missing, from both three-codes and five-codes
    val simpWordsMissingFromThreeAndFiveCodes: List[String] = stringFromTuple(missingThreeAndFive)
    simpWordsMissingFromThreeAndFiveCodes.size shouldBe 250

    val resultMapThreeCodeWords: Map[String, (OutputEntry, List[String])] = getBeyondNineSimplifiedCode(3)
    val sortedTupleListThreeCodeWords: List[(String, OutputEntry, List[String])] = getSimplifiedTuple(resultMapThreeCodeWords)
    val threeCodeWordsStringList: List[String] = stringFromTuple(sortedTupleListThreeCodeWords)
    threeCodeWordsStringList.length shouldBe 27758

    val test = ""
  }

}
