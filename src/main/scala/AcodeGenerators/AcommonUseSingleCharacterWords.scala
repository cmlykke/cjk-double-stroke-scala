package AcodeGenerators

import Adatasources.ManualData.{AcodelengthRules, AtextType}
import Atypes.{AcedictColl, AcedictEntry}

import scala.collection.immutable

object AcommonUseSingleCharacterWords {


  def getOtherCommonCharacter(commonSingleChars: Set[String],
                              SimplifiedcharSet: immutable.HashMap[String, Int],
                              TraditionalcharSet: immutable.HashMap[String, Int],
                              charFreqLimit: Int): Set[String] = {
    val simplified = SimplifiedcharSet
      .filter(x => x._2 <= AcodelengthRules.maximumCharForTwoCharCodes)
      .filter(x => !commonSingleChars.contains(x._1))
      .map(x => x._1).toSet
    val tradition = TraditionalcharSet
      .filter(x => x._2 <= AcodelengthRules.maximumCharForTwoCharCodes)
      .filter(x => !commonSingleChars.contains(x._1))
      .map(x => x._1).toSet

    return simplified ++ tradition
  }

  def getSingleCharacterWordsInCommonUse(SimplifiedcharSet: immutable.HashMap[String, Int],
                                         SimpolifiedWords: Set[AcedictEntry],
                                         SimplifiedWordStats: immutable.HashMap[String, Int],
                                         SimpToTradCharset: Map[String, Set[String]],
                                         TraditionalcharSet: immutable.HashMap[String, Int],
                                         TraditionalWords: Set[AcedictEntry],
                                         TraditionalWordStat: immutable.HashMap[String, Int],
                                         TradToSimpCharset: Map[String, Set[String]],
                                         Charsettype: AtextType , charFreqLimit: Int): Set[String] = {

    if (Charsettype == AtextType.Traditional) {
      val first10k = TraditionalWordStat.filter(x => x._2 <= charFreqLimit)
      val tradCharacters = TraditionalcharSet
        .filter(x => TraditionalWords.contains(AcedictEntry(x._1)))
        .filter(x => first10k.contains(x._1))
        .map(y => (y._1, y._2)).toList
        .sortBy(_._2)
      val result = tradCharacters.map(x => x._1).toSet

      val simplifiedVersions = result.map(x => TradToSimpCharset.get(x).get).flatten

      return simplifiedVersions ++ result
    } else if (Charsettype == AtextType.Simplified) {
      val first10k = SimplifiedWordStats.filter(x => x._2 <= charFreqLimit)
      val tradCharacters = SimplifiedcharSet
        .filter(x => SimpolifiedWords.contains(AcedictEntry(x._1)))
        .filter(x => first10k.contains(x._1))
        .map(y => (y._1, y._2)).toList
        .sortBy(_._2)
      val result = tradCharacters.map(x => x._1).toSet

      val simplifiedVersions = result.map(x => SimpToTradCharset.get(x).get).flatten

      return simplifiedVersions ++ result
    } else {
      throw new RuntimeException("unhandled textType")
    }

  }
}
