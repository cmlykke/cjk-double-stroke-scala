package staticFileGenerators

import UtilityClasses.{Cluster, ConwayColl, Grapheme, StaticFileCharInfo}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.IdsMap.GenerateNestedIdsMap
import staticFileGenerators.StaticFileGeneratorFacade.{generateConway, generateNestedIds}

class StaticFileGeneratorFacade {
  

}

object StaticFileGeneratorFacade {

  val generateConway = new GenerateConwayCodes()
  val generateNestedIds = new GenerateNestedIdsMap()
  //var charsWithInfo: scala.collection.mutable.Set[StaticFileCharInfo] =
  //  scala.collection.mutable.Set[StaticFileCharInfo]()


  def get(chrInput: Grapheme): StaticFileCharInfo = {
    val con: ConwayColl = generateConway.get(chrInput)
    val ids: List[Cluster] = generateNestedIds.get(chrInput)
    val res: StaticFileCharInfo = StaticFileCharInfo(chrInput, con, ids)
    //if (!charsWithInfo.contains(res)) {
    //charsWithInfo.add(res)
    //}
    return res
  }

  def getAll[A <: Iterable[Grapheme]](collection: A): Set[StaticFileCharInfo] = {
    val res = collection.map(x => this.get(x)).toSet
    return res
  }
}

