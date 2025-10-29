package AsortingCodes

import Adatasources.FileReaders.AreadConwayData
import AgraphemeToCodeConverters.AgraphemeToStrokeSet.generateStrokeSetMultipleConway
import Atypes.{AconwayColl, Agrapheme, AsortingCriteria}

import scala.collection.immutable.HashMap

object AsortWordsAndCharacters {

  def convertTranslatedTextToSortFormat(
                                         translateddText: Set[(String, Set[(String, AsortingCriteria)])]):
                                         Map[String, Set[(String, String, AsortingCriteria)]] = {
    val triples: Set[(String, String, AsortingCriteria)] = translateddText.flatMap {
      case (outerText, innerSet) =>
        innerSet.map {
          case (innerString, criteria) =>
            (outerText, innerString, criteria)
        }
    }
    val toSetbasedOnCodes: Map[String, Set[(String, String, AsortingCriteria)]] = triples.groupBy(_._2)
    return toSetbasedOnCodes
  }
}
