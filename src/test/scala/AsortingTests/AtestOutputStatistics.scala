package AsortingTests

import Adatasources.ManualData.AtextType
import Asingletons.AsingletonsForTests
import Atypes.{Agrapheme, AsortingObject}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AtestOutputStatistics extends AnyFlatSpec with Matchers {

  val sorted_sortCharactersSimplified: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedSimp
  val sorted_sortCharactersTraditional: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedTrad


  it should "test sorting two letters" in {

    val simplifiedSingleHits_whenSortedForSimp: Set[String] = sorted_sortCharactersSimplified
        .filter(y => y._3.graphemes.length == 1)
        .filter(x =>
        (x._3.cedictType == AtextType.Simplified) || (x._3.cedictType == AtextType.BothSimplifiedAndTraditional))
        .map(z => z._1).toSet

    val simplifiedWordHits_whenSortedForSimp: Set[String] = sorted_sortCharactersSimplified
      .filter(y => y._3.graphemes.length > 1)
      .filter(x =>
        (x._3.cedictType == AtextType.Simplified) || (x._3.cedictType == AtextType.BothSimplifiedAndTraditional))
      .map(z => z._1).toSet

    val simplifiedSingleHits_whenSortedForSimp: Set[String] = sorted_sortCharactersSimplified
      .filter(y => y._3.graphemes.length == 1)
      .filter(x =>
        (x._3.cedictType == AtextType.Simplified) || (x._3.cedictType == AtextType.BothSimplifiedAndTraditional))
      .map(z => z._1).toSet

    val simplifiedWordHits_whenSortedForSimp: Set[String] = sorted_sortCharactersSimplified
      .filter(y => y._3.graphemes.length > 1)
      .filter(x =>
        (x._3.cedictType == AtextType.Simplified) || (x._3.cedictType == AtextType.BothSimplifiedAndTraditional))
      .map(z => z._1).toSet


  }


}
