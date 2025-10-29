package Asingletons

import Adatasources.FileReaders.{AidsData, AjundaData, AreadCedictData, AreadConwayData, AtzaiData}
import Adatasources.ManualData.{AcodelengthRules, Aelements}
import Atypes.{AcedictColl, AconwayColl, Aelementstype, Agrapheme}

import scala.collection.immutable
import scala.collection.immutable.HashMap

object AsingletonsForTests {

  lazy val conwaymap: immutable.HashMap[Agrapheme, AconwayColl] = getConwayMap()
  lazy val idsmap: HashMap[Agrapheme, String] = getIdsMap()
  lazy val idsToStrokeMap: Map[String, Aelementstype] = getIdsToStrokeMap()
  lazy val basicTranslation: Map[String, String] = getBasicTranslation()
  lazy val cedict: AcedictColl = getCedict()
  lazy val junda: immutable.HashMap[String, Int] = getJunda()
  lazy val tzai: immutable.HashMap[String, Int] = getTzai()
  
  val fillCharacter: String = AcodelengthRules.fill
  
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