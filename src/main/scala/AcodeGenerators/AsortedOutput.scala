package AcodeGenerators

import Adatasources.ManualData.AtextType
import Asingletons.AsingletonsForTests
import AsortingCodes.AsortWordsAndCharacters
import Atypes.{AsortingObject, SortingCodes}

object AsortedOutput {

  private val allCodes: Set[(String, Set[(String, SortingCodes)])] = getTranslationsFromTextMultipleTextsRedone(AsingletonsForTests.chineseTextitems)

  private val output: Map[String, Set[(String, String, SortingCodes)]] = convertTranslatedTextToSortFormat(allCodes)

  val sortCharactersSimplified: List[(String, String, AsortingObject)] =
    AsortWordsAndCharacters.sortCodes(
      output,
      AtextType.Simplified)

  val sortCharactersTraditional: List[(String, String, AsortingObject)] =
    AsortWordsAndCharacters.sortCodes(
      output,
      AtextType.Traditional)
  
  private def getTranslationsFromTextMultipleTextsRedone(text: Set[String]): Set[(String, Set[(String, SortingCodes)])] = {
    val result: Set[(String, Set[(String, SortingCodes)])] = text.map(x =>
      getTranslationsFromTextRedone(x))
    return result
  }
  
  private def convertTranslatedTextToSortFormat(
                                         translateddText: Set[(String, Set[(String, SortingCodes)])]):
  Map[String, Set[(String, String, SortingCodes)]] = {
    val triples: Set[(String, String, SortingCodes)] = translateddText.flatMap {
      case (outerText, innerSet) =>
        innerSet.map {
          case (innerString, criteria) =>
            (outerText, innerString, criteria)
        }
    }
    val toSetbasedOnCodes: Map[String, Set[(String, String, SortingCodes)]] = triples.groupBy(_._2)
    return toSetbasedOnCodes
  }

  private def getTranslationsFromTextRedone(text: String): (String, Set[(String, SortingCodes)]) = {
    val result: (String, Set[(String, SortingCodes)]) = AredoneTranslation.getTranslationFromChineseString(
      text, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation
    )
    return result
  }
  
  
}
