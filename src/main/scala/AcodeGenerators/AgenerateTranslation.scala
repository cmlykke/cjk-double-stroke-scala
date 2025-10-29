package AcodeGenerators

import Atypes.PossibleWordCodes.FirstFirstFirstLastCode
import Atypes.SortingCodes.{FourCode, OneCode, SixCode}
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}

import scala.collection.immutable.HashMap

object AgenerateTranslation {

  def translationsOfSetOfStrings(hanStrings: Set[String],  
                                 conwaymap: HashMap[Agrapheme, AconwayColl],
                                 idsmap: HashMap[Agrapheme, String],
                                 idsToStrokeMap: Map[String, Aelementstype],
                                 translationMap: Map[String, String]): Set[(String, Set[(String, AsortingCriteria)])] = {
    val res = hanStrings.map(x => getTranslationFromChineseString(x, conwaymap, idsmap, idsToStrokeMap, translationMap))
    return res
  }

  def getTranslationFromChineseString(graph: String,
                                      conwaymap: HashMap[Agrapheme, AconwayColl],
                                      idsmap: HashMap[Agrapheme, String],
                                      idsToStrokeMap: Map[String, Aelementstype],
                                      translationMap: Map[String, String]): (String, Set[(String, AsortingCriteria)]) = {
    val splitStr: List[Agrapheme] = graph.codePoints().toArray.map(cp => Agrapheme(new String(Character.toChars(cp)))).toList
    var result: Set[(List[String], AsortingCriteria)] = Set();
    var finalres: Set[(String, AsortingCriteria)] = Set()
    if (splitStr.length == 1) {
      val fourCodes = AgenerateFinalSeudoCodes.seudoFourCodesFromSingleChar(
        splitStr.head,conwaymap,idsmap,idsToStrokeMap, FirstFirstFirstLastCode)
      var fourCodesNoFill: Set[(List[String], AsortingCriteria)] = AgenerateFinalSeudoCodes.getNoFillCodesFromFourCode(fourCodes)

      val sixCodes = AgenerateFinalSeudoCodes.seudoSixCodesFromSingleChar(
        splitStr.head,conwaymap,idsmap,idsToStrokeMap)
      result = fourCodes ++ fourCodesNoFill ++ sixCodes
      finalres = translate(result,translationMap, graph)
    } else if (splitStr.length != 2) {
      val fiveCodes = AgenerateFinalSeudoCodes.seudoFullWordFivecodesFromCharFourCodes(
        splitStr,conwaymap,idsmap,idsToStrokeMap)
      finalres = translate(fiveCodes,translationMap, graph)
    } else {
      val fiveCodes = AgenerateFinalSeudoCodes.seudoFullWordFivecodesFromCharFourCodes(
        splitStr, conwaymap, idsmap, idsToStrokeMap)
      finalres = translate(fiveCodes, translationMap, graph)
      val threeCodes = finalres.map(x => (x._1.toCharArray.toList.map(_.toString).take(3).mkString(""), SortingCodes.ThreeCodeTwoCharWord))
      finalres = finalres ++ threeCodes
    }

    val replacePatternWithSorting = finalres.map(x => replaceStrokePatternWithSorting(x))
    return (graph, replacePatternWithSorting)
  }

  private def replaceStrokePatternWithSorting(input: (String, AsortingCriteria)): (String, AsortingCriteria) = {
    var newSortingCode: AsortingCriteria = OneCode
    if (isSortingCode(input._2)) {
      newSortingCode = input._2
    } else if (input._2 == PossibleWordCodes.FirstFirstFirstLastCode) {
      newSortingCode = FourCode
    } else if (input._2 == PossibleWordCodes.FirstFirstFirstFirstFirstLastCode) {
      newSortingCode = SixCode
    } else {
      throw new RuntimeException("only PossibleWordCodes of length four and six should be found")
    }
    return (input._1, newSortingCode)
  }

  def isSortingCode(criteria: AsortingCriteria): Boolean = criteria match {
    case _: SortingCodes => true
    case _ => false
  }

  def isPossibleWordCode(criteria: AsortingCriteria): Boolean = criteria match {
    case _: PossibleWordCodes => true
    case _ => false
  }

  private def translate(input: Set[(List[String], AsortingCriteria)],
                translationMap: Map[String, String],
                originalString: String): Set[(String, AsortingCriteria)] = {
    val res = input.map(x => (translateSingleList(x._1, translationMap, originalString), x._2))
    return res
  }

  private def translateSingleList(codeList: List[String],
                          translationMap: Map[String, String],
                          originalString: String): String = {
    var res = ""
    for (x <- codeList ) {
      val lookup: Option[String] = translationMap.get(x)
      if (lookup.isDefined) {
        res += lookup.get
      } else {
        throw new RuntimeException("seudo code is not found in translationmap: " + x + " for for chinese word/char: " + originalString)
      }
    }
    return res
  }

}








