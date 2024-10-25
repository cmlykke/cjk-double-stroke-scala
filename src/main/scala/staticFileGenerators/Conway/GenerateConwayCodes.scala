package staticFileGenerators.Conway

import ElementGenerator.ElementList
import UtilityClasses.{Cluster, ConwayColl, Grapheme, InputSizes}
//import staticFileGenerators.IdsMap.GenerateIdsMap

import java.util
import java.util.regex.Matcher
import scala.collection.mutable
import scala.collection.mutable.HashMap

class GenerateConwayCodes {
  import GenerateConwayCodes._

  def get(CJKcharacter: Grapheme): ConwayColl = {
    conwayMap.get(CJKcharacter) match {
      case Some(lookup) => lookup
      case None => throw new NoSuchElementException("Key not found in the map: " + CJKcharacter.char)
    }
  }

  def splitSingle(inp: String): List[String] = {
    val res = inp.grouped(2).toList
    return res;
  }
  
  def expandAlternatives(str: String): Set[String] = {
    val parenmap: Map[String, String] = generateParenMap(str)
    val slashexpanded: String = expandSlashCodes(str, parenmap)
    val res: Set[String] = expandAlt(slashexpanded)
    return res
  }

  def updateMapValues(input: Map[String, String]): Map[String, String] = {
    input.map {
      case (key, value) => key -> s"($value)"
    }
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
/*
  def expandSlashCodes(str: String, parentPairs: Map[String, String]): String = {
    val pattern = "\\\\([0-9])".r

    val res = pattern.findFirstMatchIn(str) match {
      case Some(m) =>
        val slashCode = m.group(1).toInt
        val parentPair = parentPairs.getOrElse(slashCode.toString, throw new NoSuchElementException(s"Slash code $slashCode not found"))
        expandSlashCodes(str.replaceFirst(pattern.regex, java.util.regex.Matcher.quoteReplacement(parentPair)), parentPairs)
      case None => str
    }
    return res
  }*/

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

  def generateParenMap(input: String): Map[String, String] = {
    val pattern = "\\((.*?)\\)".r
    val basicmap = pattern.findAllIn(input).matchData.zipWithIndex.map {
      case (m, i) => (i + 1).toString -> m.group(1)
    }.toMap
    val res = updateMapValues(basicmap)
    return res
  }


  def adaptToInputSize(input: List[String], inputSize: InputSizes): Set[List[String]] = {
    if (inputSize.eq(InputSizes.Two_one)) {
      twoOneOnlyForElementCalculation(input)
    }else if (inputSize.eq(InputSizes.Three_one)) {
      threeOne(input)
    }else if (inputSize.eq(InputSizes.Five_one)) {
      fiveOne(input)
    } else if (inputSize.eq(InputSizes.Three_oneAndFive_one)) {
      threeOneOrFiveOne(input)
    } else {
      return Set()
    }
  }

  private def twoOneOnlyForElementCalculation(list: List[String]): Set[List[String]] = {
    list.size match
      case s if s < 4 => Set(list)
      case s if s >= 4 => Set(getFirstPairsAndLastTwo(list.take(2), getSecondLastElement(list), list.last))//Set(list.take(2) :+ list.last)
  }

  private def threeOne(list: List[String]): Set[List[String]] = {
    list.size match
      case s if s < 5 => Set(list)
      case s if s >= 5 => Set(getFirstPairsAndLastTwo(list.take(3), getSecondLastElement(list), list.last))
  }

  private def fiveOne(list: List[String]): Set[List[String]] = {
    list.size match
      case s if s < 7 => Set(list)
      case s if s >= 7 => Set(getFirstPairsAndLastTwo(list.take(5), getSecondLastElement(list), list.last))
  }

  private def threeOneOrFiveOne(list: List[String]): Set[List[String]] = {
    list.size match
      case s if s < 1 => throw new Exception("no elements in list")
      case s if s < 5 => Set(list)
      case s if s == 5 => Set(list, getFirstPairsAndLastTwo(list.take(3), getSecondLastElement(list), list.last))
      case s if s == 6 => Set(list, getFirstPairsAndLastTwo(list.take(3), getSecondLastElement(list), list.last))
      case _ => Set(getFirstPairsAndLastTwo(list.take(5), getSecondLastElement(list), list.last),
        getFirstPairsAndLastTwo(list.take(3), getSecondLastElement(list), list.last))
  }

  private def getFirstPairsAndLastTwo(initials: List[String], secondToLast: String, last: String): List[String] = {
    if (last.length == 2) {
      return initials :+ last
    } else if (last.length == 1 && secondToLast.length == 2) {
      return initials :+ (secondToLast.last.toString + last)
    } else {
      throw new Exception("incorrect elements in the last grapheme conwey")
    }
  }

  private def getSecondLastElement[A](list: List[A]): A = {
    if (list.length >= 2) {
      list(list.length - 2)
    } else {
      throw new Exception("too few elements in list")//None // return None if list has fewer than 2 elements
    }
  }

  def generateAllConway(): HashMap[Grapheme, ConwayColl] = {
    val basicConway = GenerateConwayCodes.conwayFilePath
    val orderedfile = GenerateConwayCodes.orderedMissingConway
    val readconway = new ReadConwayData()
    val basicconwayMap: mutable.HashMap[Grapheme, ConwayColl] =
    readconway.mapConwayData(basicConway)
    val missingconwayMap: mutable.HashMap[Grapheme, ConwayColl] =
    readconway.mapConwayData(orderedfile)
    val elementCharacters: mutable.HashMap[Grapheme, ConwayColl] =
      readconway.generateCowayCollMapFromElement(ElementList.elementTypes)

    val mergedMap = mutable.HashMap[Grapheme, ConwayColl]()
    mergedMap ++= basicconwayMap
    mergedMap ++= missingconwayMap
    mergedMap ++= elementCharacters
    mergedMap
  }

}

object GenerateConwayCodes {
  // compute the idsMap and nestedIdsMap here
  val conwayFilePath = "src/main/scala/staticFileGenerators/staticFiles/codepoint-character-sequence.txt"
  val cedictCharsMissingFromConway = "src/main/scala/staticFileGenerators/Conway/failed.txt"
  val orderedMissingConway = "src/main/scala/staticFileGenerators/Conway/orderedMissingConway.txt"
  
  val conwayMap: HashMap[Grapheme, ConwayColl] = new GenerateConwayCodes().generateAllConway()
  val conwaySet: Set[Grapheme] = conwayMap.keys.toSet
}


