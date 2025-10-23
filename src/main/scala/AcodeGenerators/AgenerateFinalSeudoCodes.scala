package AcodeGenerators

import Adatasources.ManualData.AcodelengthRules
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.PossibleWordCodes.{FirstCode, FirstLastCode, FirstSecondLastCode}
import Atypes.SortingCodes.{FiveCode, FourCode}
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes, SortingCodes}

import scala.collection.immutable.HashMap

object AgenerateFinalSeudoCodes {

  def seudoFourCodesFromSingleChar(graph: Agrapheme,
                                   conwaymap: HashMap[Agrapheme, AconwayColl],
                                   idsmap: HashMap[Agrapheme, String],
                                   idsToStrokeMap: Map[String, Aelementstype],
                                   codeStructure: PossibleWordCodes):
  Set[(List[String], AsortingCriteria)] = {
    var splitcodes: Set[(List[String], Int)] = Set()
    if (!conwaymap.contains(graph)) {
      return Set((List(AcodelengthRules.fill), SortingCodes.OneCode))
    } else if (!idsmap.contains(graph)) {
      return Set((List(AcodelengthRules.fill), SortingCodes.OneCode))
    } else {
      splitcodes = AgenerateElemAndRemainderLists.getsplitFourCodesfromchar(
        graph,
        conwaymap,
        idsmap,
        idsToStrokeMap
      )  
    }
    
    val seudocodes: Set[(List[String], AsortingCriteria)] =
      splitcodes.map(x => AgenerateSeudoCodes.splitCodeListSingleChar(x, codeStructure))
    return seudocodes
  }
  
  def getNoFillCodesFromFourCode(input: Set[(List[String], AsortingCriteria)]):
                                  Set[(List[String], AsortingCriteria)] = {
    val res = input.map(x => getNoFillHelper(x))
    return res
  }

  
  private def getNoFillHelper(input: (List[String], AsortingCriteria)): (List[String], AsortingCriteria) = {
    if (input._1.length == 0) {
      throw new RuntimeException("no code can be of length 0")
    }
    if (input._1.head == AcodelengthRules.fill) {
      return input
    }
    if (input._1.last != AcodelengthRules.fill) {
      if (input._1.length == 1) {
        return (input._1, SortingCodes.OneCode)
      } else if (input._1.length == 2) {
        return (input._1, SortingCodes.TwoCode)
      } else if (input._1.length == 3) {
        return (input._1, SortingCodes.ThreeCode)
      } else if (input._1.length == 4) {
        return input
      } else {
        throw new RuntimeException("No Fill codeLength length cant be greater than FourCode")
      }
    }
    return getNoFillHelper((input._1.init, input._2))
  }
  
  

  def seudoSixCodesFromSingleChar(graph: Agrapheme,
                                  conwaymap: HashMap[Agrapheme, AconwayColl],
                                  idsmap: HashMap[Agrapheme, String],
                                  idsToStrokeMap: Map[String, Aelementstype]):
  Set[(List[String], AsortingCriteria)] = {
    if (!conwaymap.contains(graph) || !idsmap.contains(graph)) {
      return Set((List(AcodelengthRules.fill), SortingCodes.OneCode))
    }
    val splitcodes: Set[(List[String], Int)] = AgenerateElemAndRemainderLists.getsplitSixCodesfromchar(
      graph,
      conwaymap,
      idsmap,
      idsToStrokeMap
    )
    val seudocodes: Set[(List[String], AsortingCriteria)] =
      splitcodes.map(x => AgenerateSeudoCodes.splitCodeListSingleChar(x, PossibleWordCodes.FirstFirstFirstFirstFirstLastCode))
    return seudocodes
  }

  def seudoFullWordFivecodesFromCharFourCodes(graph: List[Agrapheme],
                                              conwaymap: HashMap[Agrapheme, AconwayColl],
                                              idsmap: HashMap[Agrapheme, String],
                                              idsToStrokeMap: Map[String, Aelementstype]): Set[(List[String], AsortingCriteria)]  = {
    val structureList: List[PossibleWordCodes] = codeStructureHandler(graph.length)
    val FourCodesFromChars: List[Set[(List[String], AsortingCriteria)]] =
      graph.zip(structureList).map(x => seudoFourCodesFromSingleChar(x._1, conwaymap, idsmap, idsToStrokeMap, x._2))
    val moveFillCodesToTheEnd: List[Set[(List[String], AsortingCriteria)]] = FourCodesFromChars.map(x => moveFillCodesToEnd(x))
    val getCombinations: Set[List[String]] = fiveCodeCombinations(moveFillCodesToTheEnd, 5)
    val addSorting: Set[(List[String], AsortingCriteria)] = getCombinations
      .map(x => (x ++ List.fill(5 - x.length)(AcodelengthRules.fill), FiveCode))
    return addSorting//addSorting
  }

  private def moveFillCodesToEnd(addSorting: Set[(List[String], AsortingCriteria)]): Set[(List[String], AsortingCriteria)] = {
    val moved: Set[(List[String], AsortingCriteria)] = addSorting.map(x => moveFillCodesToEndHelper(List(), x, x)).toSet
    return moved
  }

  private def moveFillCodesToEndHelper(newList: List[String],
                                       input: (List[String], AsortingCriteria),
                                       original: (List[String], AsortingCriteria)): (List[String], AsortingCriteria) = {
    val newCharsContentIsComplete = newList == original._1.filter(x => !(x == AcodelengthRules.fill))
    if (original._1.length == 1 && original._1.head == AcodelengthRules.fill) {
      return original
    } else if (newCharsContentIsComplete) {
      val targetLength = original._1.length
      val updated: List[String] = newList
      return (updated, original._2)
    } else if (input._1.length > 0 && input._1.head == AcodelengthRules.fill){
      return moveFillCodesToEndHelper(newList, (input._1.tail , input._2), original)
    } else if (input._1.length > 0 && !(input._1.head == AcodelengthRules.fill)) {
      return moveFillCodesToEndHelper(newList :+ input._1.head, (input._1.tail , input._2), original)
    } else {
      throw new RuntimeException("unexpected state in moveFillCodesToEndHelper")
    }
  }


  private def codeStructureHandler(input: Int): List[PossibleWordCodes] = {
    if (input == 2) {
      return List(FirstLastCode, FirstSecondLastCode)
    } else if (input == 3) {
      return List(FirstCode, FirstLastCode, FirstLastCode)
    } else if (input == 4) {
      return List(FirstCode, FirstCode, FirstCode, FirstLastCode)
    } else if (input > 4) {
      return List(FirstCode, FirstCode, FirstCode, FirstCode, FirstCode)
    }
    throw new RuntimeException("permutations of multiple chars is not supposed to take aruments of less than 2")
  }


  private def fiveCodeCombinations(input: List[Set[(List[String], AsortingCriteria)]], targetLength: Int): Set[List[String]] = {
    val fourcodesNoSorting: List[Set[List[String]]] =
      input.map(charCodeSet => charCodeSet.map(codeTupple => codeTupple._1))
    val sudoCodeToPermutasionSet: Set[List[String]] = generatePermutations(fourcodesNoSorting, targetLength)
    return sudoCodeToPermutasionSet
  }

  private def generatePermutations(input: List[Set[List[String]]], targetLength: Int): Set[List[String]] = {
    var resultNested: Set[List[String]] = generatePermutationsHelper(Set(), input, input)
    val fillUp: Set[List[String]] = resultNested.map(x => x ++ List.fill(targetLength - x.length)(AcodelengthRules.fill))
    return resultNested
  }

  private def generatePermutationsHelper(result: Set[List[String]],
                                         input: List[Set[List[String]]],
                                         original: List[Set[List[String]]]): Set[List[String]] = {
    if (input.size == 0) {
      return result
    }
    if (result.size == 0) {
      generatePermutationsHelper(input.head, input.tail, original)
    }
    else {
      val updatedList: Set[List[String]] = input.head.map(x => result.map(y => y ++ x)).flatten
      generatePermutationsHelper(updatedList, input.tail, original)
    }
  }
}
