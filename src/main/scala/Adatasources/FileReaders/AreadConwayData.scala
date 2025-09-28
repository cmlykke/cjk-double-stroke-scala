package Adatasources.FileReaders

import Atypes.{Aconway, AconwayColl, Agrapheme}
import UtilityClasses.{ConwayColl, Grapheme}
import staticFileGenerators.Conway.GenerateConwayCodes

import scala.collection.mutable.HashMap
import scala.io.Source

object AreadConwayData {

  def conwaySetFunc(): Set[String] = {
    val conwayMap: HashMap[Agrapheme, AconwayColl] = mapConwayData()
    conwayMap.values.map(x => x.char.char).toSet
  }
  
  def mapConwayData(): HashMap[Agrapheme, AconwayColl] = {
    val basicConway = GenerateConwayCodes.conwayFilePath
    val bufferedSource = Source.fromFile(basicConway)
    val lines = bufferedSource.getLines

    val customConway = GenerateConwayCodes.orderedMissingConway
    val bufferedSourceCustom = Source.fromFile(customConway)
    val linesCustom = bufferedSourceCustom.getLines

    var resultMap = new HashMap[Agrapheme, AconwayColl]()

    for (line <- lines) {
      //val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      if (line.startsWith("U+")) {
        val splitLine = line.split("\\s")
        val field1 = splitLine(0)
        val field2raw = splitLine(1)
        val field2 = field2raw.replaceAll("[\\p{ASCII}]", "")

        if (Grapheme.isGrapheme(field2)) {
          val restOfTheFields: Aconway = Aconway(splitLine.drop(2).toList)
          resultMap.put(Agrapheme(field2), AconwayColl(restOfTheFields, field1, Agrapheme(field2)))
        } else {
          print(field1)
        }
      }
    }

    for (line <- linesCustom) {
      if (line.startsWith("U+")) {
        val splitLine = line.split("\\s")
        val field1 = splitLine(0)
        val field2raw = splitLine(1)
        val field2 = field2raw.replaceAll("[\\p{ASCII}]", "")

        if (Grapheme.isGrapheme(field2)) {
          val restOfTheFields: Aconway = Aconway(splitLine.drop(2).toList)
          resultMap.put(Agrapheme(field2), AconwayColl(restOfTheFields, field1, Agrapheme(field2)))
        } else {
          print(field1)
        }
      }
    }

    bufferedSource.close()
    bufferedSourceCustom.close()

    resultMap
  }
}
