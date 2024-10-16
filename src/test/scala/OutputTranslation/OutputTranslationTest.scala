package OutputTranslation

import OverlapCalc.OverlapCalculations
import UtilityClasses.{CedictEntry, CharSystem, ConwayUnambigous, Grapheme, OutputEntry, StaticFileCharInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable

class OutputTranslationTest extends AnyFlatSpec with Matchers {
  /*
  it shoud "translate conway to OutputEntry with the same criteria as cedict" in {
    xxx - write the needed code in OutputTranslation
  }*/
  
  it should "check two character is firstLast-firstSecondLast" in {
    val cedictRaw: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
    val cedictres: Set[OutputEntry] = OutputTranslation.outputCedict

    val ceRaw: CedictEntry = cedictRaw.filter(x => x.chineseStr == "過早").toList.head
    val getEntryList: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "過早").toList
    val myentry: OutputEntry = getEntryList(0)
    val old1: Set[String] = ceRaw.unambigous.lift(0).get.map(x => x.conwayPairs.mkString("")).toSet
    val old2: Set[String] = ceRaw.unambigous.lift(1).get.map(x => x.conwayPairs.mkString("")).toSet
    old1 == Set("nxnl","njjl","nfnl")
    old2 == Set("ngf")
    myentry.codes == Set("nlngf")
  }

  it should "check three character is first-firstLast-firstLast" in {

    val cedictRaw: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
    val cedictres: Set[OutputEntry] = OutputTranslation.outputCedict

    val ceRaw: CedictEntry = cedictRaw.filter(x => x.chineseStr == "亞原子").toList.head
    val getEntryList: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "亞原子").toList
    val myentry: OutputEntry = getEntryList(0)
    val old1: Set[String] = ceRaw.unambigous.lift(0).get.map(x => x.conwayPairs.mkString("")).toSet
    val old2: Set[String] = ceRaw.unambigous.lift(1).get.map(x => x.conwayPairs.mkString("")).toSet
    val old3: Set[String] = ceRaw.unambigous.lift(2).get.map(x => x.conwayPairs.mkString("")).toSet
    old1 == Set("jmgn")
    old2 == Set("kugo")
    old3 == Set("fh")
    myentry.codes == Set("jkofh")
  }

  it should "check four character is first-first-first-firstLast" in {

    val cedictRaw: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
    val cedictres: Set[OutputEntry] = OutputTranslation.outputCedict

    val ceRaw: CedictEntry = cedictRaw.filter(x => x.chineseStr == "執法如山").toList.head
    val getEntryList: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "執法如山").toList
    val myentry: OutputEntry = getEntryList(0)
    val old1: Set[String] = ceRaw.unambigous.lift(0).get.map(x => x.conwayPairs.mkString("")).toSet
    val old2: Set[String] = ceRaw.unambigous.lift(1).get.map(x => x.conwayPairs.mkString("")).toSet
    val old3: Set[String] = ceRaw.unambigous.lift(2).get.map(x => x.conwayPairs.mkString("")).toSet
    val old4: Set[String] = ceRaw.unambigous.lift(3).get.map(x => x.conwayPairs.mkString("")).toSet
    old1 == Set("fsyo","fsyl")
    old2 == Set("wgxl")
    old3 == Set("kfh")
    old4 == Set("nx")
    myentry.codes == Set("fwknx")
  }

  it should "check long character is first-first-first-first-first" in {

    val cedictRaw: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
    val cedictres: Set[OutputEntry] = OutputTranslation.outputCedict

    val ceRaw: List[CedictEntry] = cedictRaw.filter(x => x.chineseStr == "银喉长尾山雀").toList
    val getEntryList: List[OutputEntry] = cedictres.filter(x => x.chineseStr == "银喉长尾山雀").toList
    val myentry: OutputEntry = getEntryList(0)
    val old1: Set[String] = ceRaw.head.unambigous.lift(0).get.map(x => x.conwayPairs.mkString("")).toSet
    val old2: Set[String] = ceRaw.head.unambigous.lift(1).get.map(x => x.conwayPairs.mkString("")).toSet
    val old3: Set[String] = ceRaw.head.unambigous.lift(2).get.map(x => x.conwayPairs.mkString("")).toSet
    val old4: Set[String] = ceRaw.head.unambigous.lift(3).get.map(x => x.conwayPairs.mkString("")).toSet
    val old5: Set[String] = ceRaw.head.unambigous.lift(4).get.map(x => x.conwayPairs.mkString("")).toSet
    old1 == Set("ygmo")
    old2 == Set("ndno")
    old3 == Set("yl")
    old4 == Set("high")
    old5 == Set("nx")
    myentry.codes == Set("ynyhn")
  }

}
