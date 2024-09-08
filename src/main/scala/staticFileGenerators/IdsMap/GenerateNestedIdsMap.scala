
package staticFileGenerators.IdsMap

import UtilityClasses.{Cluster, Grapheme}

import scala.collection.mutable.HashMap

class GenerateNestedIdsMap {
  import GenerateNestedIdsMap._

  def getIdsMap: HashMap[Grapheme, List[Cluster]] = idsMap
  def getNestedIdsMap: HashMap[Grapheme, List[Cluster]] = nestedIdsMap

  def get(CJKcharacter: Grapheme): List[Cluster] = {
    if(nestedIdsMap.contains(CJKcharacter)) {
      nestedIdsMap(CJKcharacter)
    } else {
      val updatedInfo: List[Grapheme] = recursiveUpdateOfNestedIds(CJKcharacter)
      val str: String = updatedInfo.map(_.char).mkString
      val result = List(new Cluster(str))
      GenerateNestedIdsMap.synchronized {
        nestedIdsMap(CJKcharacter) = result
      }
      result
    }
  }

  private def recursiveUpdateOfNestedIds(CJKcharacter: Grapheme): List[Grapheme] = {
    getGraphemes(List(CJKcharacter), idsMap)
  }

  def getGraphemes(graphemes: List[Grapheme], idsMap: HashMap[Grapheme, List[Cluster]]): List[Grapheme] = graphemes match {
    case Nil => Nil
    case List(g) if idsMap.contains(g) && idsMap(g).headOption.exists(_.graphemes.length > 1) =>
      idsMap(g).head.graphemes.flatMap(g => getGraphemes(List(g), idsMap))
    case List(g) if idsMap.contains(g) && idsMap(g).headOption.exists(_.graphemes.length == 1) =>
      idsMap(g).head.graphemes
    case g :: rest => g :: getGraphemes(rest, idsMap)
    case _ => Nil
  }
}

object GenerateNestedIdsMap {
  // compute the idsMap and nestedIdsMap here
  val idsMap: HashMap[Grapheme, List[Cluster]] = new GenerateIdsMap().mapIdsData() // assuming GenerateIdsMap exists and has method mapIdsData
  val nestedIdsMap: HashMap[Grapheme, List[Cluster]] = HashMap[Grapheme, List[Cluster]]()
}
