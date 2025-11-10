package AsortingTests

import AcodeGenerators.{AredoneTranslation, AsortedOutput}
import Adatasources.FileReaders.{AidsData, AreadCedictData, AreadConwayData}
import Adatasources.ManualData.{AcodelengthRules, Aelements, AtextType}
import Asingletons.AsingletonsForTests
import AsortingCodes.AsortWordsAndCharacters
import Atypes.{AcedictColl, AcedictEntry, Aelementstype, Agrapheme, AsortingCriteria, AsortingObject, PossibleWordCodes, SortingCodes}
import GenerateOutput.GenerateOutputStrings
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*

class AtestSortingExamplesOrdering extends AnyFlatSpec with Matchers {

  val sorted_sortCharactersSimplified: List[(String, String, AsortingObject)] = AsortedOutput.sortCharactersSimplified

  it should "test sortingWorks" in {

    val kg_codes = sorted_sortCharactersSimplified.filter(x => x._2 == "kg")

    val allOrdered = kg_codes.map(x => x._1 + ", " + x._2 + ", " + x._3.sortingString).mkString("-")

    allOrdered shouldBe 
      "万, kg, Cri:2.CedictPrim:1.00322,.Word:9999999,.CedictSec:1.04033,.Word:9999999,.万-" +
        "兀, kg, Cri:2.CedictPrim:1.02859,.Word:0016338,.CedictSec:1.03743,.Word:9999999,.兀-" +
        "尢, kg, Cri:2.CedictPrim:1.06516,.Word:0074614,.CedictSec:1.05865,.Word:9999999,.尢-" +
        "ㄤ, kg, Cri:2.CedictPrim:1.99999,.Word:0186175,.CedictSec:1.99999,.Word:9999999,.ㄤ-" +
        "兀, kg, Cri:2.CedictPrim:2.99999,.Word:0293231,.CedictSec:2.04782,.Word:9999999,.兀-" +
        "⺎, kg, Cri:2.CedictPrim:2.99999,.Word:9999999,.CedictSec:2.99999,.Word:9999999,.⺎-" +
        "⺐, kg, Cri:2.CedictPrim:2.99999,.Word:9999999,.CedictSec:2.99999,.Word:9999999,.⺐"

  }
}
