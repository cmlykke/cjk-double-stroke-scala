package AcodeGenerators

import Atypes.SortingCodes.FourCode
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AsortingCriteria, PossibleWordCodes}

import scala.collection.immutable.HashMap

object AgenerateFinalSeudoCodes {

  def seudoFullWordFivecodesFromCharFourCodes(graph: List[Agrapheme],
                                              conwaymap: HashMap[Agrapheme, AconwayColl],
                                              idsmap: HashMap[Agrapheme, String],
                                              idsToStrokeMap: Map[String, Aelementstype]): Set[List[String]] = {
    val FourCodesFromChars: List[Set[(List[String], AsortingCriteria)]] =
      graph.map(x => seudoFourCodesFromSingleChar(x, conwaymap, idsmap, idsToStrokeMap))
    val getCombinations: Set[List[String]] = fiveCodeCombinations(FourCodesFromChars)
    return getCombinations
  }

  def fiveCodeCombinations(input: List[Set[(List[String], AsortingCriteria)]]): Set[List[String]] = {
    val fourcodesNoSorting: List[Set[List[String]]] =
      input.map(charCodeSet => charCodeSet.map(codeTupple => codeTupple._1))
    val sudoCodeToPermutasionSet: Set[List[String]] = generatePermutations(fourcodesNoSorting)
    return sudoCodeToPermutasionSet
  }

  def generatePermutations(input: List[Set[List[String]]]): Set[List[String]] = {
    if (input.size == 2) {
      return permutationsFromTwoChars(input)
    } else if (input.size == 3) {
      return permutationsFromThreeChars(input)
    } else if (input.size == 4) {
      return permutationsFromFourChars(input)
    } else if (input.size > 4) {
      return permutationsFromFiveOrMoreChars(input)
    }
    throw new RuntimeException("permutations of multiple chars is not supposed to take aruments of less than 2")
  }

  private def permutationsFromTwoChars(input: List[Set[List[String]]]): Set[List[String]] = {
    val smallCodesFirst: Set[List[String]] = input(0).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstLastCode))
    val smallCodesSecond: Set[List[String]] = input(1).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstSecondLastCode))
    val result = cartesianConcat(List(smallCodesFirst, smallCodesSecond))
    return result
  }

  private def permutationsFromThreeChars(input: List[Set[List[String]]]): Set[List[String]] = {
    val charOne: Set[List[String]] = input(0).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstCode))
    val charTwo: Set[List[String]] = input(1).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstLastCode))
    val charThree: Set[List[String]] = input(2).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstLastCode))
    val result = cartesianConcat(List(charOne, charTwo, charThree))
    return result
  }

  private def permutationsFromFourChars(input: List[Set[List[String]]]): Set[List[String]] = {
    val charOne: Set[List[String]] = input(0).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstCode))
    val charTwo: Set[List[String]] = input(1).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstCode))
    val charThree: Set[List[String]] = input(2).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstCode))
    val charFour: Set[List[String]] = input(3).map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstLastCode))
    val result = cartesianConcat(List(charOne, charTwo, charThree, charFour))
    return result
  }

  private def permutationsFromFiveOrMoreChars(input: List[Set[List[String]]]): Set[List[String]] = {
    val firstFiveChars: List[Set[List[String]]] = input.take(5).map(eachCharSet => eachCharSet.map(codeList =>
      AgenerateSeudoCodes.splitCodeListMultiChar(codeList, PossibleWordCodes.FirstCode)))
    val result = cartesianConcat(firstFiveChars)
    return result
  }

  def cartesianConcat(lists: List[Set[List[String]]]): Set[List[String]] =
    lists.foldLeft(Set(List())) { (acc, list) =>
      for {
        prefix <- acc
        suffix <- list
      } yield prefix ++ suffix
  }

  def seudoFourCodesFromSingleChar(graph: Agrapheme,
                               conwaymap: HashMap[Agrapheme, AconwayColl],
                               idsmap: HashMap[Agrapheme, String],
                               idsToStrokeMap: Map[String, Aelementstype]):
                               Set[(List[String], AsortingCriteria)] = {
    val splitcodes: Set[(List[String], Int)] = AgenerateElemAndRemainderLists.getsplitFourCodesfromchar(
      graph,
      conwaymap,
      idsmap,
      idsToStrokeMap
    )
    val seudocodes: Set[(List[String], AsortingCriteria)] =
      AgenerateSeudoCodes.convertElemAndRemainderToSeudoSingleChar(splitcodes)
    return seudocodes
  }

  def seudoSixCodesFromSingleChar(graph: Agrapheme,
                                   conwaymap: HashMap[Agrapheme, AconwayColl],
                                   idsmap: HashMap[Agrapheme, String],
                                   idsToStrokeMap: Map[String, Aelementstype]):
  Set[(List[String], AsortingCriteria)] = {
    val splitcodes: Set[(List[String], Int)] = AgenerateElemAndRemainderLists.getsplitSixCodesfromchar(
      graph,
      conwaymap,
      idsmap,
      idsToStrokeMap
    )
    val seudocodes: Set[(List[String], AsortingCriteria)] =
      AgenerateSeudoCodes.convertElemAndRemainderToSeudoSingleChar(splitcodes)
    return seudocodes
  }
}
