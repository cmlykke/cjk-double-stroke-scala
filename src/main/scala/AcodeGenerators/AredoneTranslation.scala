package AcodeGenerators

import Adatasources.ManualData.AcodelengthRules
import Asingletons.AsingletonsForTests
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}

import scala.collection.immutable.HashMap

object AredoneTranslation {


  def getTranslationFromChineseString(graph: String,
                                      conwaymap: HashMap[Agrapheme, AconwayColl],
                                      idsmap: HashMap[Agrapheme, String],
                                      idsToStrokeMap: Map[String, Aelementstype],
                                      translationMap: Map[String, String]): (String, Set[(String, SortingCodes)]) = {
    if (graph.codePoints().count() > 1) {
      return ("",Set())
    } else if (graph.codePoints().count() ==  1) {
      val seudoCodes: (String, Set[(List[String], AsortingCriteria)]) = singleCharacterCodes(graph,conwaymap,idsmap,idsToStrokeMap,translationMap)
      val result = translateSeudoCodes(seudoCodes, translationMap)
      return result
    } else {
      throw new RuntimeException("graph should not be an empty string")
    }
  }

  private def translateSeudoCodes(seudo: (String, Set[(List[String], AsortingCriteria)]),
                                  translationMap: Map[String, String]):
                                  (String, Set[(String, SortingCodes)]) = {
    var resultSet: Set[(String, SortingCodes)] = Set()
    for (eachTupple <- seudo._2) {
      var outputString: String = ""
      for (eachStr <- eachTupple._1) {
        val translation: Option[String] = translationMap.get(eachStr)
        if (translation.isEmpty) {
          throw new RuntimeException("translation not found from seudocode")
        }
        outputString += translation.get
      }
      val updateSortingAndFillZCodes: (String, SortingCodes) = getUpdateSortingAndFillZCodes(outputString, eachTupple._2)
      resultSet = resultSet ++ Set(updateSortingAndFillZCodes)
    }
    return (seudo._1, resultSet)
  }

  private def getUpdateSortingAndFillZCodes(text: String, sorting: AsortingCriteria): (String, SortingCodes) = {
    val fillCodeNumber: Int = sorting.code - text.length
    if (fillCodeNumber < 0) {
      throw new RuntimeException("code length and target codelength missmatch")
    }
    if (sorting == PossibleWordCodes.FirstFirstFirstLastCode) {
      return (text + (AcodelengthRules.fill * fillCodeNumber), SortingCodes.FourCode)
    } else if (sorting == PossibleWordCodes.FirstFirstFirstFirstFirstLastCode) {
      return (text + (AcodelengthRules.fill * fillCodeNumber), SortingCodes.SixCode)
    } else if (sorting.isInstanceOf[SortingCodes]) {
      val s: SortingCodes = sorting.asInstanceOf[SortingCodes]
      return (text, s)
    } else {
      throw new RuntimeException("unhandled exception")
    }
  }

  private def singleCharacterCodes( graph: String,
                                    conwaymap: HashMap[Agrapheme, AconwayColl],
                                    idsmap: HashMap[Agrapheme, String],
                                    idsToStrokeMap: Map[String, Aelementstype],
                                    translationMap: Map[String, String]): (String, Set[(List[String], AsortingCriteria)]) = {
    val singleCodesWithInitial: Set[(List[String], AsortingCriteria)]   =
      getCodesFromSingleWithInitial(Agrapheme(graph),conwaymap,idsmap,idsToStrokeMap,PossibleWordCodes.FirstFirstFirstLastCode)
    val singleCodesWihoutInitial: Set[(List[String], AsortingCriteria)]   =
      getCodesFromSingleWithoutInitial(Agrapheme(graph), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)
    val resultSeudoCodes: Set[(List[String], AsortingCriteria)] = (singleCodesWithInitial ++ singleCodesWihoutInitial)

    return (graph, resultSeudoCodes)
  }

  private def getCodesFromSingleWithInitial(graph: Agrapheme,
                                           conwaymap: HashMap[Agrapheme, AconwayColl],
                                           idsmap: HashMap[Agrapheme, String],
                                           idsToStrokeMap: Map[String, Aelementstype], 
                                           codeStructure: PossibleWordCodes): Set[(List[String], AsortingCriteria)] = {
    val elemAndRemainder: Set[List[String]] = AredoneSeudocodes.getElementAndRemainder(
      graph, conwaymap, idsmap, idsToStrokeMap) // Set[(List[String], AsortingCriteria)] 
    val fourCodes: Set[(List[String], AsortingCriteria)] = AredoneSeudocodes.getfourCodeSeudoCodes(
      graph,elemAndRemainder, codeStructure, codeStructure)
    //seudoFourCodesFromSingleChar
    
    var fourCodesNoFill: Set[(List[String], AsortingCriteria)] = AredoneSeudocodes.getNoFillCodesFromFourCode(fourCodes)
    return fourCodes ++ fourCodesNoFill
  }

  private def getCodesFromSingleWithoutInitial(graph: Agrapheme,
                                           conwaymap: HashMap[Agrapheme, AconwayColl],
                                           idsmap: HashMap[Agrapheme, String],
                                           idsToStrokeMap: Map[String, Aelementstype],
                                           codeStructure: PossibleWordCodes): Set[(List[String], AsortingCriteria)] = {

    val sixCodes = AredoneSeudocodes.seudoSixCodesFromSingleChar(
      graph, conwaymap)
    return sixCodes
  }


}
