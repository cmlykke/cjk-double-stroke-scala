package Atypes

import AcodeGenerators.ArollOutConway
import Adatasources.FileReaders.AidsData
import AgraphemeToCodeConverters.AgraphemeToStrokeSet

import scala.collection.immutable.HashMap

case class AidsRecur(rawEntry: Agrapheme, 
                     rawIdsMap: HashMap[Agrapheme, String], 
                     conwaymap: HashMap[Agrapheme, AconwayColl],
                     originalConway: List[String]) {
  val recurNested: List[AidsRecur] = AidsRecur.recur(rawEntry, rawIdsMap, conwaymap, originalConway)
  val rawids: String = recurNested.map(eachrecur => eachrecur.grapheme.char).mkString("")
  val grapheme: Agrapheme = rawEntry
}

object AidsRecur {

  val shapes: Set[Agrapheme] = "⿰⿱⿲⿳⿴⿵⿶⿷⿸⿹⿺⿻⿼⿽⿾⿿".map(x => Agrapheme(x.toString)).toSet
  val roadshape: Set[Agrapheme] = "⿺".map(x => Agrapheme(x.toString)).toSet
  val roadelems: Set[Agrapheme] = "辶⻎⻍⻌廴乙".map(x => Agrapheme(x.toString)).toSet
  
  val widthtrippleshape: Set[Agrapheme] = "⿲".map(x => Agrapheme(x.toString)).toSet

  def findElementmatch(graph: Agrapheme,
                       idsmap: HashMap[Agrapheme, String],
                       idsToStrokeMap: Map[String, Aelementstype],
                       backslashCleaned: String,
                       localrecur: AidsRecur
                      ): Option[String] = {
    if (graph.char == "誠") {
      val test = ""
    }
    val res = findElementmatchHelper(graph, localrecur, idsToStrokeMap, backslashCleaned)
    val verifiedMatch: Option[String] = verifyConwayMatch(graph, backslashCleaned, res,idsToStrokeMap)
    if (verifiedMatch.isDefined) {
      return res
    }else {
      return None
    }
  }

  private def verifyConwayMatch(graph: Agrapheme,
                                backslashCleaned: String,
                                res: Option[String],
                                idsToStrokeMap: Map[String, Aelementstype]): Option[String] = {
    if (res.isEmpty) {
      return None
    }
    val getStrokes = idsToStrokeMap.get(res.get)
    if (getStrokes.isEmpty) {
      return None
    }
    val rolloutMainConway: Set[String] = ArollOutConway.roolOutConway(backslashCleaned)
    val elemCoway: Set[String] = getStrokes.get.strokes.map(x => ArollOutConway.roolOutConway(x)).flatten

    val bestMatch = ArollOutConway.returnMostLikelyMatch(rolloutMainConway.toList,elemCoway.toList,graph,res)
    if (bestMatch._1.startsWith(bestMatch._2)) {
      return Option(bestMatch._2)
    }else {
      return None
    }
  }


  def findElementmatchHelper(graph: Agrapheme,
                             input: AidsRecur,
                             idsToStrokeMap: Map[String, Aelementstype],
                             backslashCleaned: String): Option[String] = {
    idsToStrokeMap.get(input.grapheme.char) match {
      case Some(_) => Some(input.grapheme.char)
      case None =>
        idsToStrokeMap.get(input.rawids) match {
          case Some(_) => Some(input.rawids)
          case None =>
            input.recurNested
              .filterNot(recur => shapes.contains(recur.grapheme))
              .headOption
              .flatMap(recur => findElementmatchHelper(graph, recur, idsToStrokeMap, backslashCleaned))
        }
    }
  }

  def recur(input: Agrapheme,
            rawIdsMap: HashMap[Agrapheme, String],
            conwaymap: HashMap[Agrapheme, AconwayColl],
            originalConway: List[String]): List[AidsRecur] = {
    var res: List[AidsRecur] = List()
    val graphOption: Option[AconwayColl] = conwaymap.get(input)
    val backslashCleaned: Option[List[String]] = graphOption
      .map(_.rawConway.rawConway.map(AgraphemeToStrokeSet.unrollBackSlash))

    val lookup: Option[String] = rawIdsMap.get(input)
    if (!lookup.isDefined) {
      return List()
      throw Exception(input.char ++ " " ++ "not found in idsMap")
    }
    val cleanLookup: String = removeBracketedSection(lookup.get)
    val graphemes: List[String] = Agrapheme.splitIntoGraphemes(cleanLookup)
    val graphemesAdjustedForStrokeorder: List[String] =
      moveElementNotFollowingStrokes(graphemes, backslashCleaned, conwaymap, originalConway)

    if (graphemesAdjustedForStrokeorder.size > 1) {
      res = graphemesAdjustedForStrokeorder.map(x => AidsRecur(Agrapheme(x), rawIdsMap, conwaymap, originalConway))
    } else if (graphemesAdjustedForStrokeorder.size == 1) {
      res = List()
    } else {
      throw Exception(input.char ++ " " ++ "when adjusted for strokeorder, the result is empty")
    }
    res

  }

  def moveElementNotFollowingStrokes(input:  List[String],
                                     backslashCleaned: Option[List[String]],
                                     conwaymap: HashMap[Agrapheme, AconwayColl],
                                     originalConway: List[String]):  List[String] = {
    val indexOfFirstNonShape: Int = input.indexWhere(str => !shapes.contains(Agrapheme(str)))

    if (input.length < 3) {
      return input
    }
    //handle ⿺ shapes:
    if (AidsRecur.roadshape.contains(Agrapheme(input.head)) && AidsRecur.roadelems.contains(Agrapheme(input(1)))) {
      return input.lift(0).toList ++ input.drop(2) ++ input.lift(1).toList
    }
    //handle ⿲ shapes:
    val trippleHoriShapeMatching = AidsRecur.widthtrippleshape.contains(Agrapheme(input(indexOfFirstNonShape-1)))
    if (indexOfFirstNonShape > 0 && trippleHoriShapeMatching) {
      val trueFirstelem = identifyTrueFirstCharacter(input, backslashCleaned, conwaymap, originalConway)
      if (trueFirstelem == input(indexOfFirstNonShape + 1)) {
        val updatedList =
          input.updated(indexOfFirstNonShape, input(indexOfFirstNonShape + 1)).updated(indexOfFirstNonShape + 1, input(indexOfFirstNonShape))
        return updatedList
      }
    }
    return input
  }

  private def identifyTrueFirstCharacter(
                                          input: List[String],
                                          backslashCleaned: Option[List[String]],
                                          conwaymap: HashMap[Agrapheme, AconwayColl],
                                          originalConway: List[String]
                                        ): String = {
    val conwayIdsTupples: Set[(String, List[String])] = input
      .filter(x => conwaymap.contains(Agrapheme(x)))
      .map(x => (x, conwaymap.get(Agrapheme(x)).get.rawConway.rawConway)).toSet
    var longestMatch: String = ""
    var longestLength = 0
    for (eachConway <- originalConway) {
      for (eachTuple <- conwayIdsTupples) {
        for (tuppleCode <- eachTuple._2) {
          if (eachConway.startsWith(tuppleCode) && tuppleCode.length > longestLength) {
            longestLength = tuppleCode.length
            longestMatch = eachTuple._1
          }
        }
      }
    }

    return longestMatch
  }

  def removeBracketedSection(input: String): String = {
    input.replaceAll("\\[.*?\\]", "")
  }
}