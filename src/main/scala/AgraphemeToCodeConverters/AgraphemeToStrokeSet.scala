package AgraphemeToCodeConverters

import Adatasources.FileReaders.AreadConwayData
import Atypes.{Aconway, AconwayColl, Agrapheme}

import scala.collection.immutable.HashMap

object AgraphemeToStrokeSet {

  def generateStrokeSet(str: Agrapheme): Set[String] = {
    val conwaymap: HashMap[Agrapheme, AconwayColl] = AreadConwayData.mapConwayData()
    val getConwayFromMap: AconwayColl = conwaymap(str)
    val rawStrokes: List[String] = getConwayFromMap.rawConway.rawConway
    val allMultiple: List[Set[String]] = rawStrokes.map(x => generateStrokeSetMultipleConway(str, x))
    val result: Set[String] = allMultiple.flatten.toSet
    return result
  }
  
  private def generateStrokeSetMultipleConway(str: Agrapheme, rawStrokes: String): Set[String] = {
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


  def unrollBackSlash(input: String): String = {
    if (input.contains("\\")) {
      val test = ""

    }
    val test2 = unrollBackSlashHelper(List(""), input, ' ', input)
    return test2
  }
  
  private def unrollBackSlashHelper(tempres: List[String],  input: String, criticalChar: Char, original: String): String = {
    val test = ""
    if (tempres.mkString("").length == original.length) {
      return handleCompleteCharacterList(tempres, original)
    }
    if (criticalChar == '\\' && !input.head.isDigit) {
      throw new RuntimeException("Backslash has to be followed by a number")
    }
    if (criticalChar == '\\') {
      val lastListItemPlusChange: String = tempres.last + input.head.toString
      val exceptLast: List[String] = tempres.dropRight(1)
      val newTempres: List[String] = exceptLast ++ List(lastListItemPlusChange) ++ List("")
      val newInput = input.drop(1)
      return unrollBackSlashHelper(newTempres, newInput, ' ', original)
    }
    if (input.head == '(' || input.head == '\\') {
      val newTempres: List[String] = tempres ++ List(input.head.toString)
      val newInput = input.drop(1)
      return unrollBackSlashHelper(newTempres, newInput, input.head, original)
    }
    if (input.head == ')') {
      val lastListItemPlusChange: String = tempres.last + ')'.toString
      val exceptLast: List[String] = tempres.dropRight(1)
      val newTempres: List[String] = exceptLast ++ List(lastListItemPlusChange)
      val newInput = input.drop(1)
      return unrollBackSlashHelper(newTempres, newInput, ' ', original)
    }
    val lastListItemPlusChange: String = tempres.last + input.head
    val exceptLast: List[String] = tempres.dropRight(1)
    val newTempres: List[String] = exceptLast ++ List(lastListItemPlusChange)
    val newInput = input.drop(1)
    return unrollBackSlashHelper(newTempres, newInput, ' ', original)
  }

  private def handleCompleteCharacterList(tempres: List[String], origianl: String): String = {
    val parens: List[String] = tempres.filter(x => x.startsWith("("))
    var res: List[String] = List()
    for (item: String <- tempres) {
      if (item.startsWith("\\")) {
        var lookupInt: Int = 0
        var lookupResult: String = ""
        try {
          lookupInt = item.last.toString.toInt
          lookupResult = parens(lookupInt-1)
        } catch {
          case e: RuntimeException =>
            throw new RuntimeException("conway parens lookup has to be possible")
        }
        res = res.appended(lookupResult)
      } else {
        res = res.appended(item)
      }
    }
    return res.mkString("")
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
