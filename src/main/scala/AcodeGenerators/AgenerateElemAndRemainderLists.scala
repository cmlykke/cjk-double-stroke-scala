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
    val elemremovedfromcode: (List[String], String) = getRemovedCode(backslashCleaned, elemsfound, idsToStrokeMap)
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
                     idsToStrokeMap: Map[String, Aelementstype]): (List[String], String) = {
    if (!elemOpt.isDefined) {
      return (List(), rawConway)
    }
    var matchinginitial: mutable.Set[String] = mutable.Set()
    val matchstrokeset: Option[Aelementstype] = idsToStrokeMap.get(elemOpt.get)
    if (!matchstrokeset.isDefined) {
      throw new RuntimeException(elemOpt.toString ++ " not found in stroke map")
    }
    for strokeinit: String <- matchstrokeset.get.strokes do {
      if (rawConway.startsWith(strokeinit)) {
        val endofstr = rawConway.slice(strokeinit.length, rawConway.length)
        matchinginitial += endofstr
      }
    }
    val shortestSubstring: String = try {
      matchinginitial.minBy(_.length)
    } catch {
      case _: Exception => ""
    }
    return (List(matchstrokeset.get.unifiedElemet), shortestSubstring)
  }
}


