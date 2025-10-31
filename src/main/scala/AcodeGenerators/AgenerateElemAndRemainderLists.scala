package AcodeGenerators

import Adatasources.FileReaders.AidsData
import Adatasources.ManualData.Aelements
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AidsRecur}

import scala.collection.immutable.HashMap
import scala.collection.mutable

object AgenerateElemAndRemainderLists {

  def getsplitFourCodesfromchar(graph: Agrapheme,
                            conwaymap: HashMap[Agrapheme, AconwayColl],
                            idsmap: HashMap[Agrapheme, String],
                            idsToStrokeMap: Map[String, Aelementstype]): Set[(List[String], Int)] = {
    val originalConway: Option[AconwayColl] = conwaymap.get(graph)
    if (originalConway.isEmpty) {
      throw new RuntimeException("AidsRecur conway not found")
    }
    val localrecur: AidsRecur = AidsRecur(graph, idsmap, conwaymap, originalConway.get.rawConway.rawConway)
    val getConwayFromMap: AconwayColl = conwaymap(graph)
    val rawConway: List[String] = getConwayFromMap.rawConway.rawConway
    val allSplitResults: List[Set[(List[String], Int)]] = 
      rawConway.map(x => getsplitFourCodesfromcharMultipleConway(graph, x, idsmap, idsToStrokeMap, localrecur))
    val result: Set[(List[String], Int)] = allSplitResults.flatten.toSet
    return result
  }
  
  private def getsplitFourCodesfromcharMultipleConway(
                    graph: Agrapheme,
                    rawConway: String,
                    idsmap: HashMap[Agrapheme, String],
                    idsToStrokeMap: Map[String, Aelementstype],
                    localrecur: AidsRecur): Set[(List[String], Int)] = {
    val backslashCleaned: String = AgraphemeToStrokeSet.unrollBackSlash(rawConway)
    val elemsfound = AidsRecur.findElementmatch(graph, idsmap, idsToStrokeMap, backslashCleaned, localrecur)
    val elemremovedfromcode: (List[String], String) = getRemovedCode(backslashCleaned, elemsfound, idsToStrokeMap, graph)
    val unrollRemainder: Set[String] = AgraphemeToStrokeSet.expandAlt(elemremovedfromcode._2)
    val res: Set[(List[String], Int)] = unrollRemainder.map(x => (elemremovedfromcode._1.appended(x), 4))
    return res
  }

  def getsplitSixCodesfromchar(graph: Agrapheme,
                                conwaymap: HashMap[Agrapheme, AconwayColl],
                                idsmap: HashMap[Agrapheme, String],
                                idsToStrokeMap: Map[String, Aelementstype]): Set[(List[String], Int)] = {
    val getConwayFromMap: AconwayColl = conwaymap(graph)
    val rawConway: List[String] = getConwayFromMap.rawConway.rawConway
    val allSplitResults: List[Set[(List[String], Int)]] = rawConway.map(x => getsplitSixCodesfromcharMultipleConway(graph, x, idsmap, idsToStrokeMap))
    val result: Set[(List[String], Int)] = allSplitResults.flatten.toSet
    return result
  }

  private def getsplitSixCodesfromcharMultipleConway(graph: Agrapheme,
                                                     rawConway: String,
                                                     idsmap: HashMap[Agrapheme, String],
                                                     idsToStrokeMap: Map[String, Aelementstype]): Set[(List[String], Int)] = {
    val backslashCleaned: String = AgraphemeToStrokeSet.unrollBackSlash(rawConway)
    val unrollFull: Set[String] = AgraphemeToStrokeSet.expandAlt(backslashCleaned)
    val unrollFullSet: Set[(List[String], Int)] = unrollFull.map(x => (List(x), 6))
    return unrollFullSet
  }


  def getRemovedCode(rawConway: String,
                     elemOpt: Option[String],
                     idsToStrokeMap: Map[String, Aelementstype],
                     graph: Agrapheme): (List[String], String) = {
    if (!elemOpt.isDefined) {
      return (List(), rawConway)
    }
    var matchinginitial: mutable.Set[String] = mutable.Set()
    val matchstrokeset: Option[Aelementstype] = idsToStrokeMap.get(elemOpt.get)
    if (!matchstrokeset.isDefined) {
      throw new RuntimeException(elemOpt.toString ++ " not found in stroke map")
    }

    //find the remainder of conway codes
    val eachRemainder: String = removeElemPartFromCharConway(rawConway, matchstrokeset.get.strokes, graph, elemOpt)
    if (eachRemainder != rawConway && (eachRemainder.size < rawConway.size) ) {
      matchinginitial += eachRemainder
    } else {
      throw new RuntimeException("despite elem found, remainderCantbeIdentified. Main char: "
        + graph.char + " elem: " + elemOpt.get + " rawConway: " + rawConway + " elemconway: " + matchstrokeset.get.strokes.mkString("|"))
    }

    val shortestSubstring: String = try {
      matchinginitial.minBy(_.length)
    } catch {
      case _: Exception => ""
    }
    return (List(matchstrokeset.get.unifiedElemet), shortestSubstring)
  }

  def removeElemPartFromCharConway(rawConway: String,
                                   elemConway: Set[String],
                                   graph: Agrapheme,
                                   elemOpt: Option[String]): String ={
    if (elemOpt.isDefined && elemOpt.get == graph.char) {
      return ""
    }
    val anyStringThatMatch: Set[String] = elemConway.filter(x => rawConway.startsWith(x))
    if (anyStringThatMatch.size == 1) {
      val endofstr = rawConway.slice(anyStringThatMatch.head.length, rawConway.length)
      return endofstr
    } else if (anyStringThatMatch.size > 1){
      throw new RuntimeException("too many hits")
    }
    if (rawConway.startsWith("(")) {
      val firstparen: Option[String] = extractBetweenFirstParens(rawConway)
      if (firstparen.isEmpty) {
        throw new RuntimeException("no paren found")
      }
      val parencontent: List[String] = firstparen.get.split("\\|").toList
      val anyMatches = elemConway.filter(x => parencontent.contains(x))
      if (anyMatches.size == 1) {
        val removedParen = removeFirstParen(rawConway)
        if (removedParen.isDefined){
          return removedParen.get
        }else {
          throw new RuntimeException("paren not found")
        }
      } else if (anyMatches.size > 1) {
        throw new RuntimeException("too many paren hits")
      }else {
        throw new RuntimeException("elem conway not found in paren")
      }
    }

    throw new RuntimeException("unknown missing pattern")
  }

  def extractBetweenFirstParens(str: String): Option[String] = {
    val start = str.indexOf('(')
    if (start == -1) None
    else {
      val end = str.indexOf(')', start + 1)
      if (end == -1) None
      else Some(str.substring(start + 1, end))
    }
  }

  def removeFirstParen(input: String): Option[String] = {
    val start = input.indexOf(')')
    if (start == -1) {
      None
    } else {
      val result = Some(input.substring(start + 1))
      return result
    }
  }
}


