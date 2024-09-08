package ElementGenerator

import UtilityClasses.{Cluster, Conway, ConwayColl, Grapheme, StaticFileCharInfo}

//2024-08-10 - proeve at bruge denne til at teste elementer
class ElementType(inputelementVersions: String, 
                  idsWithShape: Cluster,
                  letterToRepresentElementKey: String) {

  val elementVersions: String = inputelementVersions
  val elementCodes: Cluster = idsWithShape
  val elementKeyLetter: String = letterToRepresentElementKey

  def checkIfIdsStartMatch(inputChar: StaticFileCharInfo): Boolean = {
    val allClusterStrings: Set[String] =
      inputChar.ids.map(eachCluster =>
        eachCluster.noIdsShapeCharacters().map(_.char).mkString("")).toSet
    val presentElementString: String = elementCodes.noIdsShapeCharacters().map(_.char).mkString("")
    var matches: Boolean = allClusterStrings.exists(_.startsWith(presentElementString))
    matches
  }

  def checkIfRawConwayStartMatch(inputChar: StaticFileCharInfo): Boolean = {
    val conwayColl: String = inputChar.conwayColl.rawConway.rawConway
    conwayColl.startsWith(elementVersions)
  }

}
