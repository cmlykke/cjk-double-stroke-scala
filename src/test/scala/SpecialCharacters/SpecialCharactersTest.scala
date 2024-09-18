package SpecialCharacters

import OverlapCalc.OverlapCalculations
import UtilityClasses.{CharSystem, ConwayUnambigous, Grapheme, StaticFileCharInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.SpecialCharacters.ReadSpecialCharacters

import scala.collection.mutable

class SpecialCharactersTest extends AnyFlatSpec with Matchers {

  it should "check that the special characters are not dublicates" in {
    val punct: Set[Grapheme] = ReadSpecialCharacters.punctiation
    val special: Set[Grapheme] = ReadSpecialCharacters.specialCharacters
    var dublicate: mutable.HashSet[Grapheme] = mutable.HashSet()
    for (grap <- punct) {
      if (special.contains(grap)) {
        dublicate.add(grap)
      }
    }
    dublicate.size shouldBe 0
  }

  it should "check that the special characters are not CJK" in {
    val punct: Set[Grapheme] = ReadSpecialCharacters.punctiation
    val special: Set[Grapheme] = ReadSpecialCharacters.specialCharacters
    val conway: Set[Grapheme] = GenerateConwayCodes.conwaySet

    var dublicate: mutable.HashSet[Grapheme] = mutable.HashSet()
    for (grap <- punct) {
      if (conway.contains(grap)) {//.char.head >= 13312) {
        dublicate.add(grap)
      }
    }

    for (grap <- special) {
      if (conway.contains(grap)) {//(grap.char.head >= 13312) {
        dublicate.add(grap)
      }
    }
    dublicate.size shouldBe 3
    dublicate.map(x => x.char).toSet shouldBe Set("，","：","、")
  }

}
