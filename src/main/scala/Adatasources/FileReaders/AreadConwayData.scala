package Adatasources.FileReaders

import Atypes.{Aconway, AconwayColl, Agrapheme}
import UtilityClasses.{ConwayColl, Grapheme}
import staticFileGenerators.Conway.GenerateConwayCodes
import scala.collection.mutable
import scala.collection.immutable

import scala.io.Source

object AreadConwayData {

  def conwaySetFunc(): Set[String] = {
    val conwayMap: immutable.HashMap[Agrapheme, AconwayColl] = mapConwayData()
    conwayMap.values.map(x => x.char.char).toSet
  }
  
  def mapConwayData(): immutable.HashMap[Agrapheme, AconwayColl] = {
    val basicConway = GenerateConwayCodes.conwayFilePath
    val bufferedSource = Source.fromFile(basicConway)
    val lines = bufferedSource.getLines

    val customConway = GenerateConwayCodes.orderedMissingConway
    val bufferedSourceCustom = Source.fromFile(customConway)
    val linesCustom = bufferedSourceCustom.getLines

    val manualConway = GenerateConwayCodes.cedictCharsMissingFromConway
    val bufferedSourceManual= Source.fromFile(manualConway)
    val linesManual = bufferedSourceManual.getLines

    var resultMap =  mutable.HashMap[Agrapheme, AconwayColl]()

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
    val testWan2 = resultMap.get(Agrapheme("万"))

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
    val testWan = resultMap.get(Agrapheme("万"))


    for (line <- linesManual) {
      if (!line.startsWith("#")) {
        val splitLine = line.split("\\s")
        val field2raw = splitLine(0)
        val field2 = field2raw.replaceAll("[\\p{ASCII}]", "")

        if (Grapheme.isGrapheme(field2raw)) {
          val secondAndThirdTerm = List(splitLine(1),splitLine(2))
          var restOfTheFields: Aconway = null
          if (secondAndThirdTerm.length > 0) {
            restOfTheFields = Aconway(secondAndThirdTerm)
          } else {
            throw new RuntimeException("manual conway list should not be 0")
          }
          resultMap.put(Agrapheme(field2raw), AconwayColl(restOfTheFields, "unknownUnicode", Agrapheme(field2raw)))
        } else {
          print(field2raw)
        }
      }
    }

    bufferedSource.close()
    bufferedSourceCustom.close()
    bufferedSourceManual.close()

    val finalResult: immutable.HashMap[Agrapheme, AconwayColl] = immutable.HashMap.from(resultMap)
    return finalResult
  }
}
