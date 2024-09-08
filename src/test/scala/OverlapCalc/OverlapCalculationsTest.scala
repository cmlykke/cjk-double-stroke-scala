package OverlapCalc

import UtilityClasses.InputSizes.Three_oneAndFive_one
import UtilityClasses.{CharSystem, Conway, ConwayColl, ConwayUnambigous, Grapheme, StaticFileCharInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class OverlapCalculationsTest extends AnyFlatSpec with Matchers {

  def countEntriesInMap(map: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]]): Int = {
    map.values.flatten.size
  }

  it should "check Junda - how many codes have more than 9 entries for different charset sizes" in {
    
    val test1: Set[Grapheme] = OverlapCalculations.junda6000
    val result1: OverlapCalculations = new OverlapCalculations
    val result1b: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] =
      result1.calculateOverlap(test1, CharSystem.Junda, Three_oneAndFive_one)
    val finalres:  List[(Int, List[StaticFileCharInfo])] =
      result1.getOverlap(CharSystem.Junda, result1b)

    val groupbyres = result1.getMostCommonFromMap(finalres)

    val t1 = ""

  }

  it should "check that all junda char codes number is equal to the over codes number" in {

    val test1: Set[Grapheme] = OverlapCalculations.junda6000
    val result1: OverlapCalculations = new OverlapCalculations
    val result1b: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] =
      result1.calculateOverlap(test1, CharSystem.Junda, Three_oneAndFive_one)

    //the number of characters in result1b should matches the total number of codes in test1
    val totalNumberOfCodes = countEntriesInMap(result1b)
    totalNumberOfCodes shouldBe 17322
  }


  it should "check Tzai - how many codes have more than 9 entries for different charset sizes" in {

    val test1: Set[Grapheme] = OverlapCalculations.tzai6000
    val result1: OverlapCalculations = new OverlapCalculations
    val result1b: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] =
      result1.calculateOverlap(test1, CharSystem.Tzai, Three_oneAndFive_one)

    //the number of characters in result1b should matches the total number of codes in test1
    val totalNumberOfCodes = countEntriesInMap(result1b)
    totalNumberOfCodes shouldBe 20233

  }

  it should "check that all tzai char codes number is equal to the over codes number" in {

    val test1: Set[Grapheme] = OverlapCalculations.tzai6000
    val result1: OverlapCalculations = new OverlapCalculations
    val result1b: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] =
      result1.calculateOverlap(test1, CharSystem.Tzai, Three_oneAndFive_one)
    val finalres:  List[(Int, List[StaticFileCharInfo])] =
      result1.getOverlap(CharSystem.Tzai, result1b)

    val groupbyres = result1.getMostCommonFromMap(finalres)

    val t1 = ""

  }

}
