package ElementGenerator

import UtilityClasses.{Cluster, Conway, ConwayColl, Grapheme, StaticFileCharInfo}

//2024-08-10 - proeve at bruge denne til at teste elementer
class ElementType(inputelementVersions: String, 
                  idsWithShape: Cluster,
                  letterToRepresentElementKey: String,
                  inputrawstring: String) {

  val elementVersions: String = inputelementVersions
  val elementCodes: Cluster = idsWithShape
  val elementKeyLetter: String = letterToRepresentElementKey
  val rawString: String = inputrawstring

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
  
  override def equals(obj: Any): Boolean = obj match {
    case that: ElementType => this.rawString == that.rawString
    case _ => false
  }

  override def hashCode(): Int = {
    rawString.hashCode
  }
}
