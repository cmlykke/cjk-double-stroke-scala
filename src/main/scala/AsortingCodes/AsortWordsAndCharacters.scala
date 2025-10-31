package AsortingCodes

import Adatasources.FileReaders.AreadConwayData
import Adatasources.ManualData.AtextType
import AgraphemeToCodeConverters.AgraphemeToStrokeSet.generateStrokeSetMultipleConway
import Asingletons.AsingletonsForTests
import Atypes.{AcedictColl, AcedictEntry, AconwayColl, Agrapheme, AsortingCriteria, AsortingObject}

import scala.math.Ordered.orderingToOrdered
import scala.collection.immutable
import scala.collection.immutable.HashMap

object AsortWordsAndCharacters {

  def sortCodes(
                 input: Map[String, Set[(String, String, AsortingCriteria)]],
                 cedict: AcedictColl,
                 junda: immutable.HashMap[String, Int],
                 tzai: immutable.HashMap[String, Int],
                 texttype: AtextType
               ): List[(String, String, AsortingObject)] = {
    val allSets: Set[(String, String, AsortingObject)] = input
      .map(x => sortSet(x._2, cedict, junda, tzai, texttype)).flatten.toSet

    val noncodes = allSets.filter(x => x._2 == "non")
    val nonSet = noncodes.map(x => x._1).toSet

    val sortedList: List[(String, String, AsortingObject)] = allSets.toList.sortWith { (t1, t2) =>
      t1._3.compare(t2._3) < 0
    }
    return sortedList
  }

  private def sortSet(inputSet: Set[(String, String, AsortingCriteria)],
                      cedict: AcedictColl,
                      junda: immutable.HashMap[String, Int],
                      tzai: immutable.HashMap[String, Int],
                      texttype: AtextType): Set[(String, String, AsortingObject)] = {
    val spicedTupples: Set[(String, String, AsortingObject)] = inputSet
      .map(x => handleEachTup(x,  cedict: AcedictColl,
      junda: immutable.HashMap[String, Int],
      tzai: immutable.HashMap[String, Int],
      texttype: AtextType))
    return spicedTupples
  }

  private def handleEachTup(eachTup: (String, String, AsortingCriteria),
                            cedict: AcedictColl,
                            junda: immutable.HashMap[String, Int],
                            tzai: immutable.HashMap[String, Int],
                            texttype: AtextType): (String, String, AsortingObject) = {
    val charList: Set[String] = AsingletonsForTests.wordToSingle(eachTup._1)
    val eachChar: Set[(String, (Boolean, Int, Boolean, Int))] = charList.map(x => lookupEachChar(x,cedict, junda,tzai,texttype))
    val cedictPrimaryForSorting: List[Boolean] = eachChar.map(x => x._2._1).toList
    val cedictSecondaryForSorting: List[Boolean] = eachChar.map(x => x._2._3).toList
    val charsetPrimaryForSorting: List[Int] = eachChar.map(x => x._2._2).toList
    val charsetSecondaryForSorting: List[Int] = eachChar.map(x => x._2._4).toList
    val sortingCriteriaForSorting: AsortingCriteria = eachTup._3
    val hanCharsForSorting: List[String] = eachChar.map(x => x._1).toList.sorted
    val lettercodeForSorting: String = eachTup._2
    val sortingObjects: AsortingObject =
      AsortingObject(
        cedictPrimaryForSorting,
        cedictSecondaryForSorting,
        charsetPrimaryForSorting,
        charsetSecondaryForSorting,
        sortingCriteriaForSorting,
        hanCharsForSorting,
        lettercodeForSorting)
    return (eachTup._1, eachTup._2, sortingObjects)
  }

  private def lookupEachChar(inputChar: String,
                         cedict: AcedictColl,
                         junda: immutable.HashMap[String, Int],
                         tzai: immutable.HashMap[String, Int],
                         texttype: AtextType): (String, (Boolean, Int, Boolean, Int)) = {
    var cedictPrimary: Boolean = false
    var cedictSeconday: Boolean = false
    var charsetPrimary: Int = 0
    var charsetSecondary: Int = 0
    if (texttype == AtextType.Simplified) {
      cedictPrimary = cedict.simplifiedAllHanItems.contains(AcedictEntry(inputChar))
      cedictSeconday = cedict.traditionalAllHanItems.contains(AcedictEntry(inputChar))
      charsetPrimary = junda.getOrElse(inputChar, Int.MaxValue)
      charsetSecondary = tzai.getOrElse(inputChar, Int.MaxValue)
    } else if (texttype == AtextType.Traditional) {
      cedictPrimary = cedict.traditionalAllHanItems.contains(AcedictEntry(inputChar))
      cedictSeconday = cedict.simplifiedAllHanItems.contains(AcedictEntry(inputChar))
      charsetPrimary = tzai.getOrElse(inputChar, Int.MaxValue)
      charsetSecondary = junda.getOrElse(inputChar, Int.MaxValue)
    } else {
      throw new RuntimeException("uknown textType")
    }
    return (inputChar,(cedictPrimary, charsetPrimary,cedictSeconday, charsetSecondary))
  }

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

