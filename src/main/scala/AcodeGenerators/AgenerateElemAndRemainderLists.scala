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

  private def removeElemPartFromCharConway(rawConway: String,
                                   elemConway: Set[String],
                                   graph: Agrapheme,
                                   elemOpt: Option[String]): String = {
    if ("誠" == graph.char) {
      val test = ""
    }
    if (elemConway.size == 1 && elemConway.head == "") {
      return rawConway
    }
    if (graph.char == elemOpt.get) {
      return ""
    }
    if (elemConway.size > 1) {
      val longestMatch: (String, String) = ArollOutConway.returnMostLikelyMatch(
        List(rawConway), elemConway.toList, graph, elemOpt)
      return removeElemPartFromCharConway(longestMatch._1.tail, Set(longestMatch._2.tail), graph, elemOpt)
    }
    val mostLikelyMatch: (String, String) = ArollOutConway.returnMostLikelyMatch(
      List(rawConway), elemConway.toList, graph, elemOpt)

    //no paren
    if (mostLikelyMatch._1.isEmpty || mostLikelyMatch._2.isEmpty) {
      val test = ""
    }
    if ((mostLikelyMatch._1.head != '(') && (mostLikelyMatch._1.head == mostLikelyMatch._2.head)) {
      return removeElemPartFromCharConway(mostLikelyMatch._1.tail, Set(mostLikelyMatch._2.tail), graph, elemOpt)
    }
    if (mostLikelyMatch._1.head == '(' && mostLikelyMatch._2.head == '(') { //*************
      val doubleParen: (String, String) = bothMainAndElemConwayParen(mostLikelyMatch._1,mostLikelyMatch._2,graph, elemOpt)
      return removeElemPartFromCharConway(doubleParen._1.tail, Set(doubleParen._2.tail), graph, elemOpt)
    }
    if (mostLikelyMatch._1.head == '(') {
      val mainparen: (String, String) = mainConwayparen(mostLikelyMatch._1, mostLikelyMatch._2, graph, elemOpt)
      return removeElemPartFromCharConway(mainparen._1.tail, Set(mainparen._2.tail), graph, elemOpt)
    }
    if (mostLikelyMatch._2.head == '(') {
      val elemparen: (String, String) = elemConwayParen(mostLikelyMatch._1, mostLikelyMatch._2, graph, elemOpt)
      return removeElemPartFromCharConway(elemparen._1.tail, Set(elemparen._2.tail), graph, elemOpt)
    }
    throw new RuntimeException("unknown missing pattern")
  }

  private def bothMainAndElemConwayParen(conwayInput: String,
                                         elemInput: String,
                                         graph: Agrapheme,
                                         elemOpt: Option[String]): (String, String) = {
    val mainConwayParen: Option[String] = ArollOutConway.extractBetweenFirstParens(conwayInput)
    val elemConwayParen: Option[String] = ArollOutConway.extractBetweenFirstParens(elemInput)

    val mainContentSplit: List[String] = mainConwayParen.get.split("\\|").toList
    val elemContentSplit: List[String] = elemConwayParen.get.split("\\|").toList

    val mainConwayRemoveParen: Option[String] = ArollOutConway.removeFirstParen(conwayInput)
    val elemConwayRemoveParen: Option[String] = ArollOutConway.removeFirstParen(elemInput)

    var allMainCambos: List[String] = mainContentSplit.map(x => x + mainConwayRemoveParen.get)
    var allElemCambos: List[String] = elemContentSplit.map(x => x + elemConwayRemoveParen.get)

    val longestMatch: (String, String) = ArollOutConway.returnMostLikelyMatch(allMainCambos,allElemCambos,graph,elemOpt)
    if (longestMatch == ("", "")) {
      val test2 = ""
    }
    return longestMatch
  }

  private def mainConwayparen(conwayInput: String,
                              elemInput: String,
                              graph: Agrapheme,
                              elemOpt: Option[String]): (String, String) = {
    val firstParenMainConway: List[String] = ArollOutConway.rolloutFirstParen(conwayInput)
    val longestCommonPrefix: (String, String) = ArollOutConway.returnMostLikelyMatch(firstParenMainConway, List(elemInput), graph: Agrapheme, elemOpt: Option[String])
    if (longestCommonPrefix == ("", "")) {
      val test2 = ""
    }
    return longestCommonPrefix
  }

  private def elemConwayParen(conwayInput: String,
                              elemInput: String,
                              graph: Agrapheme,
                              elemOpt: Option[String]): (String, String) = {
    val firstParenElemConway: List[String] = ArollOutConway.rolloutFirstParen(elemInput)
    val longestCommonPrefix: (String, String) = ArollOutConway.returnMostLikelyMatch(List(conwayInput), firstParenElemConway, graph: Agrapheme, elemOpt: Option[String])
    if (longestCommonPrefix == ("","")) {
      val test2 = ""
    }
    return longestCommonPrefix
  }

}


