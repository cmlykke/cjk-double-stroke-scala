package AcodeGenerators

import Adatasources.ManualData.AtextType
import Asingletons.AsingletonsForTests
import AsortingCodes.AsortWordsAndCharacters
import Atypes.{AsortingObject, SortingCodes}

object AsortedOutput {

  val allCodes: Set[(String, Set[(String, SortingCodes)])] = getTranslationsFromTextMultipleTextsRedone(AsingletonsForTests.chineseTextitems)

  val output: Map[String, Set[(String, String, SortingCodes)]] = AsortWordsAndCharacters.convertTranslatedTextToSortFormat(allCodes)

  private def getTranslationsFromTextMultipleTextsRedone(text: Set[String]): Set[(String, Set[(String, SortingCodes)])] = {
    val result: Set[(String, Set[(String, SortingCodes)])] = text.map(x =>
      getTranslationsFromTextRedone(x))
    return result
  }
  
  val sortCharactersSimplified: List[(String, String, AsortingObject)] =
    AsortWordsAndCharacters.sortCodes(
      output,
      AsingletonsForTests.cedict,
      AsingletonsForTests.junda,
      AsingletonsForTests.tzai,
      AsingletonsForTests.blcuData,
      AsingletonsForTests.sinicaData,
      AtextType.Simplified)

  def getTranslationsFromTextRedone(text: String): (String, Set[(String, SortingCodes)]) = {
    val result: (String, Set[(String, SortingCodes)]) = AredoneTranslation.getTranslationFromChineseString(
      text, AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap, AsingletonsForTests.basicTranslation
    )
    return result
  }
  
  
}
