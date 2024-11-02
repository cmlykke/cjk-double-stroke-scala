package staticFileGenerators.Conway

import UtilityClasses.{Cluster, Conway, ConwayColl, ConwayTest, Grapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.IdsMap.GenerateNestedIdsMap

import scala.collection.mutable

class GenerateConwayCodesTest  extends AnyFlatSpec with Matchers{

  "conway basic and conway missing files" should " should not have dublicates" in {
    val basicConway = GenerateConwayCodes.conwayFilePath
    //val conwayMissing = GenerateConwayCodes.cedictCharsMissingFromConway
    val orderedfile = GenerateConwayCodes.orderedMissingConway

    val readconway = new ReadConwayData()
    val basicconwayMap: mutable.HashMap[Grapheme, ConwayColl] =
      readconway.mapConwayData(basicConway)
    val missingconwayMap: mutable.HashMap[Grapheme, ConwayColl] =
      readconway.mapConwayData(orderedfile)
    val missingConwayGraphemeSet = readconway.fileToNonAsciiGraphemeSet(orderedfile)
    val mergemap = basicconwayMap.clone().addAll(missingconwayMap)
    
    basicconwayMap.keys.size shouldEqual 28095
    missingconwayMap.keys.size shouldEqual 206
    missingconwayMap.keys.size shouldEqual missingConwayGraphemeSet.size
    mergemap.keys.size shouldEqual 28301
  }
  
  "The expandAlternatives function" should "test that expansion is correct" in {
    val mapper = new GenerateConwayCodes()
    val raw1 = "3511(252|522)52"
    val raw2 = "34(152|154|454)"
    val raw3 = "(12|34)(56|78)9"
    val raw4 = "12(3|)45"
    val raw5 = "12(|3)45"

    val res1 = mapper.expandAlternatives(raw1)
    val res2 = mapper.expandAlternatives(raw2)
    val res3 = mapper.expandAlternatives(raw3)
    val res4 = mapper.expandAlternatives(raw4)
    val res5 = mapper.expandAlternatives(raw4)

    res1 shouldEqual Set("351125252", "351152252")
    res2 shouldEqual Set("34152", "34154", "34454")
    res3 shouldEqual Set("12569", "12789", "34569", "34789")
    res4 shouldEqual Set("12345", "1245")
    res5 shouldEqual Set("1245", "12345")

  }

  it should "test raw codes that cause errors" in {
    val mapper = new GenerateConwayCodes()
    val raw5 = "(1|4)111251(122|1212|2112)1\\2112"
    val res5 = mapper.expandAlternatives(raw5)
    res5.size shouldEqual 27
  }

  "The expand backslash function" should "test that expansion is correct" in {
    val mapper = new GenerateConwayCodes()
    val raw1 = "54(2511|3511|3541)(15|35|53)\\24444"
    val raw2 = "4334(122|1212|2112)1\\1112"
    val raw3 = "12(23|45)(45|67)\\212\\1"
    val raw4 = "41341(1|4)111251(554234|554444)\\23112" // U+7674	癴	41341(1|4)111251(554234|554444)\23112

    val res1 = mapper.expandAlternatives(raw1)
    val res2 = mapper.expandAlternatives(raw2)
    val res3 = mapper.expandAlternatives(raw3)
    val res4 = mapper.expandAlternatives(raw4)
    res1.size shouldEqual 27
    res2.size shouldEqual 9
    res3.size shouldEqual 16

    val parenmap1: Map[String, String] = mapper.generateParenMap(raw1)
    val slashexpanded1: String = mapper.expandSlashCodes(raw1, parenmap1)
    slashexpanded1 shouldEqual "54(2511|3511|3541)(15|35|53)(15|35|53)4444"

    val parenmap2: Map[String, String] = mapper.generateParenMap(raw2)
    val slashexpanded2: String = mapper.expandSlashCodes(raw2, parenmap2)
    slashexpanded2 shouldEqual "4334(122|1212|2112)1(122|1212|2112)112"

    val parenmap3: Map[String, String] = mapper.generateParenMap(raw3)
    val slashexpanded3: String = mapper.expandSlashCodes(raw3, parenmap3)
    slashexpanded3 shouldEqual "12(23|45)(45|67)(45|67)12(23|45)"
  }

  "The GenerateNestedIdsMap function" should "correctly generate a map with replaced graphemes" in {
    val mapper = new GenerateConwayCodes()

    var cluster1List: ConwayColl = mapper.get(Grapheme("朏"))
    var cluster1: Conway = cluster1List.rawConway
    cluster1.rawConway shouldBe "3511(252|522)52"

    var cluster2List: ConwayColl = mapper.get(Grapheme("令"))
    var cluster2: Conway = cluster2List.rawConway
    cluster2.rawConway shouldBe "34(152|154|454)"
  }
//令	34(152|154|454)

  "Conway chars test" should "test the size of the conway char set" in {
    val conwayChars = GenerateConwayCodes.conwaySet
    conwayChars.size == 28095
  }
/*
  "Conway value test" should "test the number of lines in Coll" in {
    val mapper = new GenerateConwayCodes()
    val conwayChars = mapper.getConwayCharacters
    val allValues: Set[ConwayColl] = mapper.getConwayMap.values.toSet

    val alltworaw: Set[ConwayColl] = allValues.filter(x => x.rawConwayCollection.length != 1)

    conwayChars.size == 28095
  }

 */

}
