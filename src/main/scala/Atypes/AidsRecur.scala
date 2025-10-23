package Atypes

import Adatasources.FileReaders.AidsData

import scala.collection.immutable.HashMap

case class AidsRecur(rawEntry: Agrapheme, rawIdsMap: HashMap[Agrapheme, String]) {
  val recurNested: List[AidsRecur] = AidsRecur.recur(rawEntry, rawIdsMap)
  val rawids: String = recurNested.map(eachrecur => eachrecur.grapheme.char).mkString("")
  val grapheme: Agrapheme = rawEntry
}

object AidsRecur {

  val shapes: Set[Agrapheme] = "⿰⿱⿲⿳⿴⿵⿶⿷⿸⿹⿺⿻⿼⿽⿾⿿".map(x => Agrapheme(x.toString)).toSet
  val roadshape: Set[Agrapheme] = "⿺".map(x => Agrapheme(x.toString)).toSet
  val roadelems: Set[Agrapheme] = "辶⻎⻍⻌廴乙".map(x => Agrapheme(x.toString)).toSet
  
  val widthtrippleshape: Set[Agrapheme] = "⿲".map(x => Agrapheme(x.toString)).toSet
  val widthtrippleelems: Set[Agrapheme] = "言訁⾔".map(x => Agrapheme(x.toString)).toSet
  val sideTrippleElems: Set[Agrapheme] = "糸糹⽷⺯".map(x => Agrapheme(x.toString)).toSet

  def findElementmatch(input: Agrapheme,
                       idsmap: HashMap[Agrapheme, String],
                       idsToStrokeMap: Map[String, Aelementstype]): Option[String] = {
    val localrecur: AidsRecur = AidsRecur(input, idsmap)
    val res = findElementmatchHelper(localrecur, idsToStrokeMap)
    return res
  }


  def findElementmatchHelper(input: AidsRecur, idsToStrokeMap: Map[String, Aelementstype]): Option[String] = {
    idsToStrokeMap.get(input.grapheme.char) match {
      case Some(_) => Some(input.grapheme.char)
      case None =>
        idsToStrokeMap.get(input.rawids) match {
          case Some(_) => Some(input.rawids)
          case None =>
            input.recurNested
              .filterNot(recur => shapes.contains(recur.grapheme))
              .headOption
              .flatMap(recur => findElementmatchHelper(recur, idsToStrokeMap))
        }
    }
  }

  def recur(input: Agrapheme, rawIdsMap: HashMap[Agrapheme, String]): List[AidsRecur] = {
      var res: List[AidsRecur] = List()

      val lookup: Option[String] = rawIdsMap.get(input)
      if (!lookup.isDefined) {
        return List()
        throw Exception(input.char ++ " " ++ "not found in idsMap")
      }
      val cleanLookup: String = removeBracketedSection(lookup.get)
      val graphemes: List[String] = Agrapheme.splitIntoGraphemes(cleanLookup)
      val graphemesAdjustedForStrokeorder: List[String] = moveElementNotFollowingStrokes(graphemes)

      if (graphemesAdjustedForStrokeorder.size > 1) {
        res = graphemesAdjustedForStrokeorder.map(x => AidsRecur(Agrapheme(x), rawIdsMap))
      } else if (graphemesAdjustedForStrokeorder.size == 1) {
        res = List()
      }else {
        throw Exception(input.char ++ " " ++ "when adjusted for strokeorder, the result is empty")
      }
      res

  }

  def moveElementNotFollowingStrokes(input:  List[String]):  List[String] = {
    if (input.length < 3) {
      return input
    }
    //handle ⿺ shapes:
    if (AidsRecur.roadshape.contains(Agrapheme(input.head)) && AidsRecur.roadelems.contains(Agrapheme(input(1)))) {
      return input.lift(0).toList ++ input.drop(2) ++ input.lift(1).toList
    }
    //handle ⿲ shapes:
    if (AidsRecur.widthtrippleshape.contains(Agrapheme(input.head)) &&
      AidsRecur.widthtrippleelems.contains(Agrapheme(input(2))) &&
      AidsRecur.sideTrippleElems.contains(Agrapheme(input(1)))) {
      return input.lift(0).toList ++ input.drop(2) ++ input.lift(1).toList
    }
    return input
  }

  def removeBracketedSection(input: String): String = {
    input.replaceAll("\\[.*?\\]", "")
  }
}