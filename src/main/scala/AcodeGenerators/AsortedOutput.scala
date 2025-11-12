package AcodeGenerators

import Adatasources.ManualData.AtextType
import Asingletons.AsingletonsForTests
import ApublishingCodes.AsortWordsAndCharacters
import Atypes.{AsortingObject, SortingCodes}

object AsortedOutput {

  private val allCodes: Set[(String, Set[(String, SortingCodes)])] = getTranslationsFromTextMultipleTextsRedone(AsingletonsForTests.chineseTextitems)

  private val output: Map[String, Set[(String, String, SortingCodes)]] = convertTranslatedTextToSortFormat(allCodes)

  val sortCharactersSimplified: List[(String, String, AsortingObject)] = generateOutput(output, AtextType.Simplified)

  val sortCharactersTraditional: List[(String, String, AsortingObject)] = generateOutput(output, AtextType.Traditional)

  private def generateOutput(input: Map[String, Set[(String, String, SortingCodes)]],
                             texttype: AtextType ): List[(String, String, AsortingObject)] = {
    val tempres: List[(String, String, AsortingObject)] = AsortWordsAndCharacters.sortCodes(input,texttype)

    val seen = scala.collection.mutable.Set[String]()
    val result = scala.collection.mutable.ListBuffer[(String, String, AsortingObject)]()
    for ((s1, s2, sortingObj) <- tempres) {
      val combined = s1 + s2
      if (seen.add(combined)) { // add returns true if it was NOT already present
        result += ((s1, s2, sortingObj))
      }
    }
    result.toList
  }

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
