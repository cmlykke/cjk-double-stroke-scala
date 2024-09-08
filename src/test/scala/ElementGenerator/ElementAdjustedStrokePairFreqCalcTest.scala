package ElementGenerator

import UtilityClasses.CharSystem
import UtilityClasses.CharSystem.{Junda, Tzai}
import UtilityClasses.{ConwayUnambigous, Grapheme, StaticFileCharInfo, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.LinkedHashMap

class ElementAdjustedStrokePairFreqCalcTest extends AnyFlatSpec with Matchers {

  it should "check Junda - check the Frequency of each elements" in {
/*
    val myobj: ElementAdjustedStrokePairFreqCalc = new ElementAdjustedStrokePairFreqCalc()
    val res: Map[String, Double] = myobj.FourCodeFreqMap(Junda)

    // Create a new sorted map using mutable.LinkedHashMap to maintain order
    val sortedResByAlphabet: LinkedHashMap[String, Double] = sortByAlphabet(res)

    val sortedResByValue: LinkedHashMap[String, Double] = sortByValue(res)
    
    val str = ""*/
  }

  it should "check tzai - check the Frequency of each elements" in {
/*
    val myobj: ElementAdjustedStrokePairFreqCalc = new ElementAdjustedStrokePairFreqCalc()
    val res: Map[String, Double] = myobj.FourCodeFreqMap(Tzai)

    // Create a new sorted map using mutable.LinkedHashMap to maintain order
    val sortedResByAlphabet: LinkedHashMap[String, Double] = sortByAlphabet(res)

    val sortedResByValue: LinkedHashMap[String, Double] = sortByValue(res)

    val str = ""*/
  }

  private def sortByValue(res: Map[String, Double]): LinkedHashMap[String, Double] = {
    // Sort the map by values, keeping the keys in order
    val sortedEntries = res.toSeq.sortBy(_._2)

    // Create a new sorted map using mutable.LinkedHashMap to maintain order by value
    val sortedResByValue = LinkedHashMap(sortedEntries: _*)

    sortedResByValue
  }

  private def sortByAlphabet(res: Map[String, Double]): LinkedHashMap[String, Double] = {

    // Partition keys into letters and numbers
    val (letters, numbers) = res.keys.partition(_.forall(_.isLetter))

    // Further partition numbers into single digits and double digits
    val (singleDigits, doubleDigits) = numbers.partition(key => key.forall(_.isDigit) && key.length == 1)

    // Sort the keys accordingly
    val sortedKeys =
      letters.toList.sorted ++
        singleDigits.toList.sorted ++
        doubleDigits.toList.sorted(Ordering[Int].on[String](_.toInt))

    // Create a new sorted map using mutable.LinkedHashMap to maintain order
    val sortedResByAlphabet = LinkedHashMap(sortedKeys.map(key => key -> res(key)): _*)
    return sortedResByAlphabet
  }
}