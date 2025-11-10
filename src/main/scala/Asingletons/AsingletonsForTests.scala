package Asingletons

import AcodeGenerators.AsortedOutput
import Adatasources.FileReaders.{AblcuData, AidsData, AjundaData, AreadCedictData, AreadConwayData, AsinicaData, AtzaiData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Atypes.{AcedictColl, AcedictEntry, AconwayColl, Aelementstype, Agrapheme, AsortingCriteria, AsortingObject, PossibleWordCodes, SortingCodes}

import scala.collection.immutable
import scala.collection.immutable.HashMap
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Asingletons.AsingletonsForTests
import AsortingCodes.AsortWordsAndCharacters
import GenerateOutput.GenerateOutputStrings
import UtilityClasses.OutputEntry

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*

object AsingletonsForTests {

  lazy val conwaymap: immutable.HashMap[Agrapheme, AconwayColl] = getConwayMap()
  lazy val idsmap: HashMap[Agrapheme, String] = getIdsMap()
  lazy val idsToStrokeMap: Map[String, Aelementstype] = getIdsToStrokeMap()
  lazy val basicTranslation: Map[String, String] = getBasicTranslation()
  lazy val cedict: AcedictColl = getCedict()
  lazy val junda: immutable.HashMap[String, Int] = getJunda()
  lazy val tzai: immutable.HashMap[String, Int] = getTzai()
  lazy val chineseTextitems: Set[String] = getAllChineseTextItems()
  lazy val blcuData: immutable.HashMap[String, Int] = AblcuData.generateMapBLCUData()
  lazy val sinicaData: immutable.HashMap[String, Int] = AsinicaData.sinicaMap
  lazy val outputSortedSimp: List[(String, String, AsortingObject)] = AsortedOutput.sortCharactersSimplified
  lazy val outputSortedTrad: List[(String, String, AsortingObject)] = AsortedOutput.sortCharactersTraditional
  
  val fillCharacter: String = AcodelengthRules.fill
  
  private def getAllChineseTextItems(): Set[String] = {

    val sinplifiedCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.simplifiedWords ++ AsingletonsForTests.cedict.simplifiedAllHanItems
    val traditionalCharsAndWords: Set[AcedictEntry] = AsingletonsForTests.cedict.traditionalWords ++ AsingletonsForTests.cedict.traditionalAllHanItems

    val total: Set[String] = sinplifiedCharsAndWords.map(x => x.rawEntry) ++ traditionalCharsAndWords.map(x => x.rawEntry)
    val conwayStrings: Set[String] = AsingletonsForTests.conwaymap.map(x => x._1.char).toSet

    val allChineseStr: Set[String] = total ++ conwayStrings

    val allToSingle: Set[String] = allChineseStr.map(x => wordToSingle(x)).flatten

    val allTotal = allToSingle ++ allChineseStr
    return allTotal
  }
  
  def wordToSingle(input: String): Set[String] = {
    input
      .codePoints()
      .mapToObj(cp => new String(Character.toChars(cp)))
      .collect(java.util.stream.Collectors.toSet())
      .asScala
      .toSet
  }
  
  private def getTzai(): immutable.HashMap[String, Int] = {
    return AtzaiData.generateMapTzaiData()
  }
  
  private def getJunda(): immutable.HashMap[String, Int] = {
    return AjundaData.generateMapJundaData() 
  }
  
  private def getConwayMap(): immutable.HashMap[Agrapheme, AconwayColl] = {
    return AreadConwayData.mapConwayData()
  }
  
  private def getIdsMap(): HashMap[Agrapheme, String] = {
    return AidsData.idsDataRaw()
  }
  
  private def getIdsToStrokeMap(): Map[String, Aelementstype] = {
    return Aelements.idsToStrokeMap
  }
  
  private def getBasicTranslation(): Map[String, String] = {
    return AcodelengthRules.elementTypes
  }
  
  private def getCedict(): AcedictColl = {
    return AreadCedictData.listCedictData()  
  }
  
}


/*
object Config {
  lazy val expensiveDatabaseConnection: Database = {
    println("Connecting to DB...")  // Runs only once
    Database.connect("prod-db")
  }

  lazy val configMap: Map[String, String] = {
    println("Parsing config file...")
    ConfigParser.parse("/app.conf")
  }
}

println(Config.expensiveDatabaseConnection)  // computes
println(Config.expensiveDatabaseConnection)  // reused, no recompute
*/