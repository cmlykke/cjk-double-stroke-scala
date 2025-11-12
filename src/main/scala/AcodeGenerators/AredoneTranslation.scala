package AcodeGenerators

import Adatasources.ManualData.AcodelengthRules
import Asingletons.AsingletonsForTests
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}

import java.util.stream.Collectors
import scala.collection.immutable.HashMap

object AredoneTranslation {


  def getTranslationFromChineseString(graph: String,
                                      conwaymap: HashMap[Agrapheme, AconwayColl],
                                      idsmap: HashMap[Agrapheme, String],
                                      idsToStrokeMap: Map[String, Aelementstype],
                                      translationMap: Map[String, String]): (String, Set[(String, SortingCodes)]) = {
    if (graph.codePoints().count() > 1) {
      val codepointGraphemes: List[String] = graph.codePoints().toArray.map { cp =>
          new String(Character.toChars(cp.toInt))
        }.toList
      val seudoCodes: Set[(List[String], AsortingCriteria)] = multiCharacterCodes(codepointGraphemes,conwaymap,idsmap,idsToStrokeMap,translationMap)
      val result = translateSeudoCodes((graph, seudoCodes), translationMap)
      return result
    } else if (graph.codePoints().count() ==  1) {
      val seudoCodes: (String, Set[(List[String], AsortingCriteria)]) = singleCharacterCodes(graph,conwaymap,idsmap,idsToStrokeMap,translationMap)
      val result = translateSeudoCodes(seudoCodes, translationMap)
      val twoLetters = getTwoCodesFromSingleChars(result)
      val merge = (result._1, result._2 ++ twoLetters._2)
      return merge
    } else {
      throw new RuntimeException("graph should not be an empty string")
    }
  }

  private def getTwoCodesFromSingleChars(input: (String, Set[(String, SortingCodes)])): (String, Set[(String, SortingCodes)]) = {
    val commonFinalForWords = AsingletonsForTests.commonFinalTwoLetter
    val commonFinalForChars = AsingletonsForTests.commonFinalTwoLetterChar

    if (commonFinalForWords.contains(input._1)) {
      val usefullCodes: Set[String] = input._2
        .filter(x => x._1.size == 4)
        .filter(x => x._1.charAt(1).toString != AcodelengthRules.fill)
        .map(x => x._1)
      val twoCodes: Set[String] = usefullCodes.map(x => x.take(2))
      val updatedTupple = twoCodes.map(x => (x, SortingCodes.TwoCodeCommonSingleWords))
      return (input._1, updatedTupple)
    }else if (commonFinalForChars.contains(input._1)){
      val usefullCodes: Set[String] = input._2
        .filter(x => x._1.size == 4)
        .filter(x => x._1.charAt(1).toString != AcodelengthRules.fill)
        .map(x => x._1)
      val twoCodes: Set[String] = usefullCodes.map(x => x.take(2))
      val updatedTupple = twoCodes.map(x => (x, SortingCodes.TwoCodeCommonSingleChars))
      return (input._1, updatedTupple)
    }
    return (input._1, Set())
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

  private def multiCharacterCodes(codepointGraphemes: List[String],
                                  conwaymap: HashMap[Agrapheme, AconwayColl],
                                  idsmap: HashMap[Agrapheme, String],
                                  idsToStrokeMap: Map[String, Aelementstype],
                                  translationMap: Map[String, String]): Set[(List[String], AsortingCriteria)] = {
    if (codepointGraphemes.length == 2) {
      val firstCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(0)),conwaymap,idsmap,idsToStrokeMap,PossibleWordCodes.FirstLastCode)
      val secondCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(1)), conwaymap, idsmap, idsToStrokeMap,PossibleWordCodes.FirstSecondLastCode)
      val tempSeudo: List[Set[(List[String], AsortingCriteria)]] = List(firstCharacter, secondCharacter)
      val merge: Set[(List[String], AsortingCriteria)] = mergeCharsFromMultiWithFill(tempSeudo)
      val threeCodesToadd = generateThreeCharCodes(merge)
      return merge ++ threeCodesToadd
    } else if (codepointGraphemes.length == 3) {
      val firstCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(0)), conwaymap, idsmap, idsToStrokeMap,PossibleWordCodes.FirstCode)
      val secondCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(1)), conwaymap, idsmap, idsToStrokeMap,PossibleWordCodes.FirstLastCode)
      val thirdCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(2)), conwaymap, idsmap, idsToStrokeMap,PossibleWordCodes.FirstLastCode)
      val tempSeudo: List[Set[(List[String], AsortingCriteria)]] = List(firstCharacter, secondCharacter, thirdCharacter)
      val merge = mergeCharsFromMultiWithFill(tempSeudo)
      return merge
    } else if (codepointGraphemes.length == 4) {
      val firstCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(0)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val secondCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(1)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val thirdCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(2)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val forthCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(3)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstLastCode)
      val tempSeudo: List[Set[(List[String], AsortingCriteria)]] = List(firstCharacter, secondCharacter, thirdCharacter, forthCharacter)
      val merge = mergeCharsFromMultiWithFill(tempSeudo)
      return merge
    } else if (codepointGraphemes.length > 4) {
      val firstCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(0)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val secondCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(1)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val thirdCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(2)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val forthCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(3)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val fifthCharacter: Set[(List[String], AsortingCriteria)] = getCodesFromSingleWithInitial(Agrapheme(codepointGraphemes(4)), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstCode)
      val tempSeudo: List[Set[(List[String], AsortingCriteria)]] = List(firstCharacter, secondCharacter, thirdCharacter, forthCharacter,fifthCharacter)
      val merge = mergeCharsFromMultiWithFill(tempSeudo)
      return merge
    } else {
      throw new RuntimeException("multi character length not handled")
    }
  }

  private def generateThreeCharCodes(merged: Set[(List[String], AsortingCriteria)]):
                                    Set[(List[String], AsortingCriteria)] = {
    val results = merged.map(x => generateThreeCharCodesHelper(List(), x, x)).toSet
    return results
  }

  private def generateThreeCharCodesHelper(output: List[String],
                                           inputTupple: (List[String], AsortingCriteria),
                                           inputTuppleOriginal: (List[String], AsortingCriteria)):
                                          (List[String], AsortingCriteria) = {
    if (inputTupple._1.isEmpty || output.size == 3) {
      return (output, SortingCodes.ThreeCodeTwoCharWord)
    }
    return generateThreeCharCodesHelper(output ++ List(inputTupple._1.head), (inputTupple._1.tail, inputTupple._2), inputTuppleOriginal)
  }

  private def mergeCharsFromMultiWithFill(inputCodes: List[Set[(List[String], AsortingCriteria)]]):
                                  Set[(List[String], AsortingCriteria)] = {
    val removeRedundant: List[Set[List[String]]] = inputCodes.map(x => x.map(y => y._1))
    val zippedResult: Set[List[String]] = mergeCharsFromMultiHelper(Set(), removeRedundant, removeRedundant)
    var zippedFilled: Set[List[String]] = Set()
    for (eachZipped <- zippedResult) {
      val eachSize: Int = eachZipped.size
      val fillCharList: List[String] = (AsingletonsForTests.fillCharacter * (5 - eachSize)).map(x => x.toString).toList
      zippedFilled = zippedFilled ++ Set(eachZipped ++ fillCharList)
    }
    return zippedFilled.map(x => (x, SortingCodes.FiveCode))
  }

  private def mergeCharsFromMultiHelper(output: Set[List[String]],
                                        removeRedundant: List[Set[List[String]]],
                                        removeRedundantOriginal: List[Set[List[String]]]): Set[List[String]] = {
    if (removeRedundant.isEmpty) {
      return output
    }
    if (output.isEmpty) {
      return mergeCharsFromMultiHelper(removeRedundant.head, removeRedundant.tail, removeRedundantOriginal)
    }
    val firstSet: Set[List[String]] = removeRedundant.head
    val updatedOutput: Set[List[String]] = output.map(x => firstSet.map(y => x ++ y)).flatten
    return mergeCharsFromMultiHelper(updatedOutput, removeRedundant.tail, removeRedundantOriginal)
  }


  private def singleCharacterCodes( graph: String,
                                    conwaymap: HashMap[Agrapheme, AconwayColl],
                                    idsmap: HashMap[Agrapheme, String],
                                    idsToStrokeMap: Map[String, Aelementstype],
                                    translationMap: Map[String, String]): (String, Set[(List[String], AsortingCriteria)]) = {
    if (graph == "倗") {
      val test = ""
    }
    val singleCodesWithInitial: Set[(List[String], AsortingCriteria)]   =
      getCodesFromSingleWithInitial(Agrapheme(graph),conwaymap,idsmap,idsToStrokeMap,PossibleWordCodes.FirstFirstFirstLastCode)
    val singleCodesWihoutInitial: Set[(List[String], AsortingCriteria)]   =
      getCodesFromSingleWithoutInitial(Agrapheme(graph), conwaymap, idsmap, idsToStrokeMap, PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)
    val resultSeudoCodes: Set[(List[String], AsortingCriteria)] = (singleCodesWithInitial ++ singleCodesWihoutInitial)

    if (graph == "倗") {
      val test = ""
    }
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
