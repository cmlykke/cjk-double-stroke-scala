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

    val simplifiedSingleHits: Set[String] = sorted_sortCharactersSimplified
        .filter(y => y._3.graphemes.length == 1)
        .filter(x =>
          !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    val simplifiedWordHits: Set[String] = sorted_sortCharactersSimplified
      .filter(y => y._3.graphemes.length > 1)
      .filter(x =>
        !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    val traditionalSingleHits: Set[String] = sorted_sortCharactersTraditional
      .filter(y => y._3.graphemes.length == 1)
      .filter(x =>
        !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    val traditionalWordHits: Set[String] = sorted_sortCharactersTraditional
      .filter(y => y._3.graphemes.length > 1)
      .filter(x =>
        !(x._3.cedictType == AtextType.Neither))
      .map(z => z._1).toSet

    simplifiedSingleHits.size shouldBe 28318
    traditionalSingleHits.size shouldBe 28318

    simplifiedWordHits.size shouldBe 179753
    traditionalWordHits.size shouldBe 179753
    

  }


}
