package Atypes

import scala.collection.immutable.HashMap

case class AidsRecur(rawEntry: Agrapheme, rawIdsMap: HashMap[Agrapheme, String]) {
  val recurNested: List[AidsRecur] = AidsRecur.recur(rawEntry, rawIdsMap)
  val grapheme: Agrapheme = rawEntry
}

object AidsRecur {
  def recur(input: Agrapheme, rawIdsMap: HashMap[Agrapheme, String]): List[AidsRecur] = {
    var res: List[AidsRecur] = List()

    val lookup: Option[String] = rawIdsMap.get(input)
    if (!lookup.isDefined) {
      throw Exception(input.char ++ " " ++ "not found in idsMap")
    }
    val cleanLookup: String = removeBracketedSection(lookup.get)
    val graphemes: List[String] = Agrapheme.splitIntoGraphemes(cleanLookup)

    if (graphemes.size > 1) {
      res = graphemes.map(x => AidsRecur(Agrapheme(x), rawIdsMap))
    } else if (graphemes.head != input.char) {
      throw Exception(input.char ++ " " ++ "had single ids value that was not identical")
    }
    res
  }

  def removeBracketedSection(input: String): String = {
    input.replaceAll("\\[.*?\\]", "")
  }
}