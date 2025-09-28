package AgraphemeToCodeConverters

import Adatasources.FileReaders.AreadConwayData
import Atypes.{Aconway, AconwayColl, Agrapheme}

import scala.collection.mutable.HashMap

object AgraphemeToStrokeSet {

  def generateStrokeSet(str: Agrapheme): Set[String] = {
    val conwaymap: HashMap[Agrapheme, AconwayColl] = AreadConwayData.mapConwayData()
    val getConwayFromMap: AconwayColl = conwaymap(str)
    val rawStrokes: String = getConwayFromMap.rawConway.rawConway
    val parenmap: Map[String, String] = generateParenMap(rawStrokes)
    val slashexpanded: String = expandSlashCodes(rawStrokes, parenmap)
    val res: Set[String] = expandAlt(slashexpanded)
    return res
  }

  def generateParenMap(input: String): Map[String, String] = {
    val pattern = "\\((.*?)\\)".r
    val basicmap = pattern.findAllIn(input).matchData.zipWithIndex.map {
      case (m, i) => (i + 1).toString -> m.group(1)
    }.toMap
    val res = updateMapValues(basicmap)
    return res
  }

  def updateMapValues(input: Map[String, String]): Map[String, String] = {
    input.map {
      case (key, value) => key -> s"($value)"
    }
  }

  def expandSlashCodes(str: String, parentPairs: Map[String, String]): String = {
    val pattern = "\\\\([0-9])".r
    var result = str

    pattern.findAllMatchIn(str).foreach { m =>
      val slashCode = m.group(1)
      val parentPair = parentPairs.getOrElse(slashCode, throw new NoSuchElementException(s"Slash code $slashCode not found"))
      result = result.replaceAll("\\\\" + slashCode, parentPair)
    }

    result
  }

  def expandAlt(str: String): Set[String] = {
    val pattern = "\\(([^)]*)\\)".r

    pattern.findFirstIn(str) match {
      case Some(m) =>
        val alternatives = m.stripPrefix("(").stripSuffix(")").split("\\|", -1).toSet
        alternatives.flatMap(alt => expandAlt(str.replaceFirst(pattern.regex, java.util.regex.Matcher.quoteReplacement(alt))))
      case None => Set(str)
    }
  }
}
