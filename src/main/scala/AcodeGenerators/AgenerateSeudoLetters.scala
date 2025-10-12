package AcodeGenerators

import Adatasources.FileReaders.AidsData
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.{AconwayColl, Aelements, Aelementstype, Agrapheme, AidsRecur}

import scala.collection.immutable.HashMap
import scala.collection.mutable

class AgenerateSeudoLetters {

}

object AgenerateSeudoLetters {
  def getsplitcodesfromchar(graph: Agrapheme,
                            conwaymap: HashMap[Agrapheme, AconwayColl],
                            idsmap: HashMap[Agrapheme, String],
                            idsToStrokeMap: Map[String, Aelementstype]): Set[List[String]] = {
    val getConwayFromMap: AconwayColl = conwaymap(graph)
    val rawConway: String = getConwayFromMap.rawConway.rawConway
    val elemsfound = AidsRecur.findElementmatch(graph, idsmap, idsToStrokeMap)
    val elemremovedfromcode: (List[String], String)= getRemovedCode(rawConway, elemsfound, idsToStrokeMap)
    val unrollRemainder: Set[String] = AgraphemeToStrokeSet.expandAlt(elemremovedfromcode._2)
    val res: Set[List[String]] = unrollRemainder.map(x => elemremovedfromcode._1.appended(x))
    return res
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
    val shortestSubstring = matchinginitial.minBy(_.length)
    return (List(matchstrokeset.get.unifiedElemet), shortestSubstring)
  }
}


