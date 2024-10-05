package ElementGenerator

import UtilityClasses.InputSizes.{Five_one, Three_one, Three_oneAndFive_one, Two_one}
import UtilityClasses.{Cluster, Conway, ConwayUnambigous, Grapheme, InputSizes, StaticFileCharInfo, StaticFileCharInfoWithLetterConway}

import scala.collection.Factory  // Import the Factory type class

class ElementEditor {

  def createDecoratedCharInfo[C[X] <: Iterable[X]](
                                                    charInfos: C[StaticFileCharInfo],
                                                    elements: List[ElementType]
                                                  )(using factory: Factory[StaticFileCharInfoWithLetterConway, C[StaticFileCharInfoWithLetterConway]]
                                                  ): C[StaticFileCharInfoWithLetterConway] = {
    val newFeature = "New Feature"
    val builder = factory.newBuilder
    charInfos.foreach(charInfo => builder +=
      createNewFeature(charInfo, elements)
    )
    builder.result()
  }

  private def createNewFeature(charIfo: StaticFileCharInfo,
                               elements: List[ElementType]): StaticFileCharInfoWithLetterConway = {
    val conwayObj: Conway = charIfo.conwayColl.rawConway
    val rawConway = charIfo.conwayColl.rawConway.rawConway
    val mychar = charIfo.grapheme.char
    if ("言".equals(mychar)) {
      val test2 = ""
    }
    val idsWithShapes = charIfo.ids
    val matchingElement: Option[ElementType] = findMatchingElement(elements, rawConway, idsWithShapes)

    if (matchingElement.isDefined) {
      val elemConway = matchingElement.get.elementVersions
      val elemRolloutConway = Conway.generateConway.expandAlternatives(elemConway)
      val charRolloutConway = Conway.generateConway.expandAlternatives(rawConway)

      val substracted = substratcElemConwayFromItem(charRolloutConway, elemRolloutConway)

      //create 2-1 two char combinations that should then have the initial added
      val splitsingle: Set[List[String]] = substracted.map(x => Conway.generateConway.splitSingle(x))
      val withInputSize: Set[List[String]] = splitsingle.flatten(x => Conway.generateConway.adaptToInputSize(x, Two_one))
      val withInitial: Set[List[String]] = withInputSize.map(matchingElement.get.elementKeyLetter :: _)

      // create 5-1 two char combinations for the original rolled out conway - charRolloutConway
      val splitsingle52: Set[List[String]] = charRolloutConway.map(x => Conway.generateConway.splitSingle(x))
      val withInputSize52: Set[List[String]] = splitsingle52.flatten(x => Conway.generateConway.adaptToInputSize(x, Five_one))

      val unambigWithInitialLetters: Set[ConwayUnambigous]  =  withInitial.map(new ConwayUnambigous(_, is4Code = true))
      val unambig52: Set[ConwayUnambigous] = withInputSize52.map(new ConwayUnambigous(_, is4Code = false))
      val combined = unambig52 ++ unambigWithInitialLetters
      val res: StaticFileCharInfoWithLetterConway = new StaticFileCharInfoWithLetterConway(charIfo, combined)
      return res
    }

    val combined: Set[ConwayUnambigous] = charIfo.conwayColl.rawConway.getSplitConwayList(Three_one)
    val res = new StaticFileCharInfoWithLetterConway(charIfo, combined)
    return res
  }
  
  /*
  if (substracted == Set("")) {
        //val plainSplitSingle: Set[List[String]] = charRolloutConway.map(x => Conway.generateConway.splitSingle(x))
        //val plainFourOne: Set[List[String]] = plainSplitSingle.flatten(x => Conway.generateConway.adaptToInputSize(x, Three_one))
        //val unambigPlainFour: Set[ConwayUnambigous] = plainFourOne.map(new ConwayUnambigous(_, is4Code = true))
        res = new StaticFileCharInfoWithLetterConway(charIfo, (unambigWithInitialLetters++unambig52))
      } else {
        res = new StaticFileCharInfoWithLetterConway(charIfo, (unambigWithInitialLetters++unambig52))
      }
  */

  private def substratcElemConwayFromItem(itemConway: Set[String], elemConway: Set[String]): Set[String] = {
    var res: Set[String] = Set()
    itemConway.foreach(itemCon => {
      for (elem <- elemConway) {
        if (itemCon.startsWith(elem)) {
          val subtractedStr = itemCon.substring(elem.length)
          res = res + subtractedStr
        }
      }
    })

    res
  }

  private def findMatchingElement(elements: List[ElementType],
                                  rawConway: String,
                                  idsWithShapes: List[Cluster]): Option[ElementType] = {
    var allrelevantElements: List[ElementType] = List()
    for (elem <- elements) {
      val conwayStart: Boolean = rawConway.startsWith(elem.elementVersions)
      val idsStart: Boolean = idsStartWithElem(idsWithShapes, elem.elementCodes)
      if (conwayStart && idsStart) {
        allrelevantElements = allrelevantElements :+ elem
      }
    }
    if (allrelevantElements.length > 1) {
      //foot and mouth overlap
      val hardcodedSet: Set[String] = Set("251", "251(215|2121)")
      val elemSet: Set[String] = Set(allrelevantElements(0).elementVersions, allrelevantElements(1).elementVersions);
      if (allrelevantElements.length == 2 && elemSet == hardcodedSet) {
        if (allrelevantElements(0).elementVersions == "251(215|2121)") {
          return Some(allrelevantElements(0))
        } else {
          return Some(allrelevantElements(1))
        }
      } else {
        throw new Exception("too many element matches")
      }
    }
    return allrelevantElements.headOption
  }

  private def idsStartWithElem(clusters: List[Cluster], cluster: Cluster): Boolean = {
    var foundClusterMatch: Boolean = false
    for (eachCluster <- clusters) {
      val currentClusterMatch: Boolean = idsStartWithCluster(eachCluster, cluster)
      if (currentClusterMatch) {
        foundClusterMatch = true;
      }
    }
    return foundClusterMatch;
  }

  // Modified function to accomplish the task
  private def idsStartWithCluster(
                                   eachCharIdsCluster: Cluster,
                                   elementCluster: Cluster
                                 ): Boolean = {
    var CharClusterGraphemes: List[Grapheme] =
      eachCharIdsCluster.graphemes
    var elementClusterGraphemes: List[Grapheme] =
      elementCluster.graphemes

    def doesSublistExistWithShapeCondition(
                                            list1: List[Grapheme],
                                            list2: List[Grapheme]
                                          ): Boolean = {
      if (list2.isEmpty) {
        return true // An empty list is considered a sublist of any list
      }
      if (list1.isEmpty) {
        return false // A non-empty list2 cannot be a sublist of an empty list1
      }

      val sublistLength = list2.length

      for (i <- list1.indices) {
        // Check if there's enough room for list2 to be a sublist of list1 starting at i
        if (i + sublistLength <= list1.length) {
          val sublistCandidate = list1.slice(i, i + sublistLength)
          if (sublistCandidate == list2) {
            // list2 matches this position in list1, now check the preceding elements
            val precedingElements = list1.slice(0, i)
            val allPrecedingShapes = precedingElements.forall(_.isShape)
            if (allPrecedingShapes) {
              return true
            }
          }
        }
      }
      false
    }

    // Use the helper function to check for the sublist condition
    doesSublistExistWithShapeCondition(CharClusterGraphemes, elementClusterGraphemes)
  }

}