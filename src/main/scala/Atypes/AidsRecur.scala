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

  val elementTypes: Set[Aelementstype] = Aelements.elementTypes
  val idsToStrokeMap:  Map[String, Set[String]] = Aelements.idsToStrokeMap

  def findElementmatch(input: Agrapheme, idsmap: HashMap[Agrapheme, String]): Option[String] = {
    val localrecur: AidsRecur = AidsRecur(input, idsmap)
    val res = findElementmatchHelper(localrecur)
    return res
  }
  
  
  def findElementmatchHelper(input: AidsRecur): Option[String] = {
    idsToStrokeMap.get(input.grapheme.char) match {
      case Some(_) => Some(input.grapheme.char)
      case None =>
        idsToStrokeMap.get(input.rawids) match {
          case Some(_) => Some(input.rawids)
          case None =>
            input.recurNested
              .filterNot(recur => shapes.contains(recur.grapheme))
              .headOption
              .flatMap(findElementmatchHelper)
        }
    }
  }

  def recur(input: Agrapheme, rawIdsMap: HashMap[Agrapheme, String]): List[AidsRecur] = {
    var res: List[AidsRecur] = List()

    val lookup: Option[String] = rawIdsMap.get(input)
    if (!lookup.isDefined) {
      throw Exception(input.char ++ " " ++ "not found in idsMap")
    }
    val cleanLookup: String = removeBracketedSection(lookup.get)
    val graphemes: List[String] = Agrapheme.splitIntoGraphemes(cleanLookup)
    val graphemesAdjustedForStrokeorder: List[String] = moveElementNotFollowingStrokes(graphemes)

    if (graphemesAdjustedForStrokeorder.size > 1) {
      res = graphemesAdjustedForStrokeorder.map(x => AidsRecur(Agrapheme(x), rawIdsMap))
    } else if (graphemesAdjustedForStrokeorder.head != input.char) {
      throw Exception(input.char ++ " " ++ "had single ids value that was not identical")
    }
    res
  }

  def moveElementNotFollowingStrokes(input:  List[String]):  List[String] = {
    if (input.length < 3) {
      return input
    }
    //handle ⿺ shapes:
    if (roadshape.contains(Agrapheme(input.head)) && roadelems.contains(Agrapheme(input(1)))) {
      return input.lift(0).toList ++ input.drop(2) ++ input.lift(1).toList
    }
    //handle ⿲ shapes:
    if (widthtrippleshape.contains(Agrapheme(input.head)) && widthtrippleelems.contains(Agrapheme(input(2)))) {
      return input.lift(0).toList ++ input.drop(2) ++ input.lift(1).toList
    }
    return input
  }

  def removeBracketedSection(input: String): String = {
    input.replaceAll("\\[.*?\\]", "")
  }
}