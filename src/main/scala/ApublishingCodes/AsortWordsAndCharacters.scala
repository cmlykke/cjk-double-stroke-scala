package ApublishingCodes

import Adatasources.FileReaders.AreadConwayData
import Adatasources.ManualData.AtextType
import AgraphemeToCodeConverters.AgraphemeToStrokeSet.generateStrokeSetMultipleConway
import Asingletons.AsingletonsForTests
import Atypes.{AcedictColl, AcedictEntry, AconwayColl, Agrapheme, AsortingCriteria, AsortingObject, SortingCodes}

import scala.math.Ordered.orderingToOrdered
import scala.collection.immutable
import scala.collection.immutable.HashMap

object AsortWordsAndCharacters {

  def sortCodes(
                 input: Map[String, Set[(String, String, SortingCodes)]],
                 texttype: AtextType 
               ): List[(String, String, AsortingObject)] = {
    val allSets: Set[(String, String, AsortingObject)] = input
      .map(x => sortSet(x._2, texttype)).flatten.toSet

    val sortedList: List[(String, String, AsortingObject)] = allSets.toList.sortWith {
      (t1, t2) =>
        val byLength = t1._2.length.compare(t2._2.length)
        if (byLength != 0) byLength < 0
        else {
          val byString = t1._2.compare(t2._2)
          if (byString != 0) byString < 0
          else t1._3.compare(t2._3) < 0
        }
    }
    sortedList
  }

  private def sortSet(inputSet: Set[(String, String, SortingCodes)],
                      texttype: AtextType): Set[(String, String, AsortingObject)] = {
    val spicedTupples: Set[(String, String, AsortingObject)] = inputSet
      .map(x => (x._1, x._2 ,AsortingObject(x._1, x._3, texttype)))
    return spicedTupples
  }

}

