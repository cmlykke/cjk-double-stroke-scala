package staticFileGenerators

import UtilityClasses.{Grapheme, StaticFileCharInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.JundaFrequency.GenerateJundaMap


class StaticFileGeneratorFacadeTest extends AnyFlatSpec with Matchers
{

  "test StaticFileGeneratorFacade get " should "single grapheme should have all needed info" in {
    //val conway: Set[Grapheme] = GenerateConwayCodes.conwayChars
    val firstChar = new Grapheme("暴")
    val facade = new StaticFileGeneratorFacade()
    val get01: StaticFileCharInfo = facade.get(firstChar)
    get01 should not be null

    val test = ""
  }

  "test StaticFileGeneratorFacade getAll " should "all graphemes should have all needed info" in {
    //val conway: Set[Grapheme] = GenerateConwayCodes.conwayChars
    val conway: Set[Grapheme] = GenerateConwayCodes.conwayCharsAll
    val facade = new StaticFileGeneratorFacade()
    val get01: Set[StaticFileCharInfo] = facade.getAll(conway)
    get01 should not be null

    val test = ""
  }

  "test junda and tzai content in conway" should "all junda and tzai should apear in conway" in {
    val conway: Set[Grapheme] = GenerateConwayCodes.conwayCharsAll

    var jundaSet: Set[Int] = conway
      .filter(x => x.junda.isDefined)
      .map(x => x.junda.get.ordinal).toSet

    val allIntegersSetJunda: Set[Int] = Set.range(1, 9934)
    val resultSetJunda: List[Int] = (allIntegersSetJunda -- jundaSet).toList.sorted

    var tzaiSet: Set[Int] = conway
      .filter(x => x.tzai.isDefined)
      .map(x => x.tzai.get.ordinal).toSet

    val allIntegersSetTzai: Set[Int] = Set.range(1, 13061)
    val resultSetTzai: List[Int] = (allIntegersSetTzai -- tzaiSet).toList.sorted

    //test that these 4 chars are not missing from conway
    //junda -- 8220, 9019  裏  秊
    //tzai -- 4782, 9574  兀  嗀

    resultSetJunda.length shouldEqual 0
    resultSetTzai.length shouldEqual 0

  }
}
