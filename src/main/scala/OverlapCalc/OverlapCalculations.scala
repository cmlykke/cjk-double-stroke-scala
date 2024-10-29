package OverlapCalc

import UtilityClasses.{CharSystem, ConwayColl, ConwayUnambigous, Grapheme, InputSizes, StaticFileCharInfo, StaticFileCharInfoWithLetterConway}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.StaticFileGeneratorFacade
import scala.annotation.targetName
import scala.collection.mutable

class OverlapCalculations {
  import OverlapCalculations._

  /** Helper method to handle common logic for processing overlap.
   *
   * @param test
   *   Set of StaticFileCharInfo
   * @param charSystem
   *   Character system
   * @param inputSize
   *   Input sizes
   * @return
   *   Overlap calculations as a map
   */
  private def processOverlap(
                              test: Set[StaticFileCharInfo],
                              charSystem: CharSystem,
                              inputSize: InputSizes
                            ): mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] = {
    implicit val staticFileCharInfoOrdering: Ordering[StaticFileCharInfo] = charSystem match {
      case CharSystem.Junda =>
        Ordering.by(_.grapheme.junda.map(_.ordinal).getOrElse(Int.MaxValue))
      case CharSystem.Tzai =>
        Ordering.by(_.grapheme.tzai.map(_.ordinal).getOrElse(Int.MaxValue))
    }

    val allUnambigousMap: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] = mutable.Map.empty

    test.foreach { item =>
      processItem(item, allUnambigousMap, inputSize)
    }

    allUnambigousMap
  }

  private def processOverlapDecorated(
                              test: Set[StaticFileCharInfoWithLetterConway],
                              charSystem: CharSystem,
                              inputSize: InputSizes
                            ): mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]] = {
    implicit val staticFileCharInfoOrdering: Ordering[StaticFileCharInfoWithLetterConway] = charSystem match {
      case CharSystem.Junda =>
        Ordering.by(_.grapheme.junda.map(_.ordinal).getOrElse(Int.MaxValue))
      case CharSystem.Tzai =>
        Ordering.by(_.grapheme.tzai.map(_.ordinal).getOrElse(Int.MaxValue))
    }

    val allUnambigousMap: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]] = mutable.Map.empty

    test.foreach { item =>
      processItem(item, allUnambigousMap, inputSize)
    }

    allUnambigousMap
  }

  @targetName("calculateOverlapWithFeature")
  def calculateOverlapDecorated(
                        input: Set[StaticFileCharInfoWithLetterConway],
                        charSystem: CharSystem,
                        inputSize: InputSizes
                      ): mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]] = {
    processOverlapDecorated(input, charSystem, inputSize)
  }

  @targetName("calculateOverlapWithGrapheme")
  def calculateOverlap(
                        input: Set[Grapheme],
                        charSystem: CharSystem,
                        inputSize: InputSizes
                      ): mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]] = {
    val test: Set[StaticFileCharInfo] = StaticFileGeneratorFacade.getAll(input)
    processOverlap(test, charSystem, inputSize)
  }

  private def processItem(
                           item: StaticFileCharInfo,
                           allUnambigousMap: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]],
                           inputSize: InputSizes
                         ): Unit = {
    try {
      val collUnique: Set[ConwayUnambigous] = item.conwayColl.rawConway.getSplitConwayList(inputSize)
      collUnique.foreach { unambique =>
        val entry: mutable.Set[StaticFileCharInfo] = allUnambigousMap.getOrElse(unambique, mutable.Set.empty)
        entry.addOne(item)
        allUnambigousMap.addOne(unambique, entry)
      }
    } catch {
      case e: Exception => e.printStackTrace() // Handle the exception appropriately
    }
  }

  private def processItem(
                           item: StaticFileCharInfoWithLetterConway,
                           allUnambigousMap: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]],
                           inputSize: InputSizes
                         ): Unit = {
    try {
      val collUnique: Set[ConwayUnambigous] = item.letterConway
      collUnique.foreach { unambique =>
        val entry: mutable.Set[StaticFileCharInfoWithLetterConway] = allUnambigousMap.getOrElse(unambique, mutable.Set.empty)
        entry.addOne(item)
        allUnambigousMap.addOne(unambique, entry)
      }
    } catch {
      case e: Exception => e.printStackTrace() // Handle the exception appropriately
    }
  }
  
  def getMostCommonFromMap(
                            input: List[(Int, List[StaticFileCharInfo])]
                          ): Map[Grapheme, Int] = {
    val values: List[StaticFileCharInfo] = input.flatMap(_._2)
    getMostCommonIds(values)
  }

  def getMostCommonIds(
                        input: List[StaticFileCharInfo]
                      ): Map[Grapheme, Int] = {
    val graph = getFirstIdsFromAllClustersInAllChars(input)
    countGraphemeOccurrences(graph)
  }

  private def countGraphemeOccurrences(
                                        graphemes: List[Grapheme]
                                      ): Map[Grapheme, Int] = {
    graphemes
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap
  }

  private def getFirstIdsFromAllClustersInAllChars(
                                                    input: List[StaticFileCharInfo]
                                                  ): List[Grapheme] = {
    input.flatMap(getFirstIdsFromCharInfoCluster)
  }

  private def getFirstIdsFromCharInfoCluster(
                                              input: StaticFileCharInfo
                                            ): List[Grapheme] = {
    input.ids.map(_.noIdsShapeCharacters().head).toList
  }

  def getOverlap(
                  charSystem: CharSystem,
                  allUnambigousMap: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]]
                ): List[(Int, List[StaticFileCharInfo])] = {

    implicit val listStringOrdering: Ordering[List[String]] =
      Ordering.fromLessThan(_.mkString < _.mkString)
    implicit val staticFileCharInfoOrdering: Ordering[ConwayUnambigous] =
      Ordering.by(_.conwayPairs)

    var resultList: List[(Int, List[StaticFileCharInfo])] = List()

    allUnambigousMap.foreach { case (_, value) =>
      val sorted = sortListByCharSystem(value.toList, charSystem)
      if (sorted.length > 9) {
        resultList = charSystem match {
          case CharSystem.Junda =>
            resultList :+ (sorted(9).grapheme.junda.get.ordinal, sorted)
          case CharSystem.Tzai =>
            resultList :+ (sorted(9).grapheme.tzai.get.ordinal, sorted)
        }
      }
    }

    resultList.sortBy(_._1)
  }

  def getOverlapDecorated(
                  charSystem: CharSystem,
                  allUnambigousMap: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]]
                ): List[(Int, List[StaticFileCharInfoWithLetterConway])] = {

    implicit val listStringOrdering: Ordering[List[String]] =
      Ordering.fromLessThan(_.mkString < _.mkString)
    implicit val staticFileCharInfoOrdering: Ordering[ConwayUnambigous] =
      Ordering.by(_.conwayPairs)

    var resultList: List[(Int, List[StaticFileCharInfoWithLetterConway])] = List()

    allUnambigousMap.foreach { case (_, value) =>
      val sorted = sortListByCharSystemDecorated(value.toList, charSystem)
      if (sorted.length > 9) {
        resultList = charSystem match {
          case CharSystem.Junda =>
            resultList :+ (sorted(9).grapheme.junda.get.ordinal, sorted)
          case CharSystem.Tzai =>
            resultList :+ (sorted(9).grapheme.tzai.get.ordinal, sorted)
        }
      }
    }

    resultList.sortBy(_._1)
  }

  def sortListByCharSystem(
                                     input: List[StaticFileCharInfo],
                                     systemEnum: CharSystem
                                   ): List[StaticFileCharInfo] = {
    systemEnum match {
      case CharSystem.Junda => sortListByJunda(input)
      case CharSystem.Tzai => sortListByTzai(input)
    }
  }
  
  def sortListByCharSystemDecorated(
                            input: List[StaticFileCharInfoWithLetterConway],
                            systemEnum: CharSystem
                          ): List[StaticFileCharInfoWithLetterConway] = {
    systemEnum match {
      case CharSystem.Junda => sortListByJundaDecorated(input)
      case CharSystem.Tzai  => sortListByTzaiDecorated(input)
    }
  }

  def sortListByJundaDecorated(input: List[StaticFileCharInfoWithLetterConway]): List[StaticFileCharInfoWithLetterConway] = {
    input.sortWith {
      case (a, b) =>
        (a.grapheme.junda, b.grapheme.junda) match {
          case (Some(jundaA), Some(jundaB)) => jundaA.ordinal < jundaB.ordinal
          case (Some(_), None) => true
          case (None, Some(_)) => false
          case (None, None) => true
        }
    }
  }
  
  def sortListByJunda(input: List[StaticFileCharInfo]): List[StaticFileCharInfo] = {
    input.sortWith {
      case (a, b) =>
        (a.grapheme.junda, b.grapheme.junda) match {
          case (Some(jundaA), Some(jundaB)) => jundaA.ordinal < jundaB.ordinal
          case (Some(_), None)              => true
          case (None, Some(_))              => false
          case (None, None)                 => true
        }
    }
  }
//
  def sortListByTzaiDecorated(input: List[StaticFileCharInfoWithLetterConway]): List[StaticFileCharInfoWithLetterConway] = {
    input.sortWith {
      case (a, b) =>
        (a.grapheme.tzai, b.grapheme.tzai) match {
          case (Some(tzaiA), Some(tzaiB)) => tzaiA.ordinal < tzaiB.ordinal
          case (Some(_), None) => true
          case (None, Some(_)) => false
          case (None, None) => true
        }
    }
  }
  
  def sortListByTzai(input: List[StaticFileCharInfo]): List[StaticFileCharInfo] = {
    input.sortWith {
      case (a, b) =>
        (a.grapheme.tzai, b.grapheme.tzai) match {
          case (Some(tzaiA), Some(tzaiB)) => tzaiA.ordinal < tzaiB.ordinal
          case (Some(_), None)            => true
          case (None, Some(_))            => false
          case (None, None)               => true
        }
    }
  }
}

object OverlapCalculations {
  val allGraphemes: Set[Grapheme] = GenerateConwayCodes.conwaySet
  val staticfile = StaticFileGeneratorFacade()

  val junda8000: Set[Grapheme] = allGraphemes.filter(x =>
    x.junda.isDefined && x.junda.get.ordinal <= 8000
  ).toSet
  val junda7000: Set[Grapheme] = allGraphemes.filter(x =>
    x.junda.isDefined && x.junda.get.ordinal <= 7000
  ).toSet
  val junda6000: Set[Grapheme] = allGraphemes.filter(x =>
    x.junda.isDefined && x.junda.get.ordinal <= 6000
  ).toSet
  val junda5000: Set[Grapheme] = allGraphemes.filter(x =>
    x.junda.isDefined && x.junda.get.ordinal <= 5000
  ).toSet
  val junda4000: Set[Grapheme] = allGraphemes.filter(x =>
    x.junda.isDefined && x.junda.get.ordinal <= 4000
  ).toSet
  val junda3000: Set[Grapheme] = allGraphemes.filter(x =>
    x.junda.isDefined && x.junda.get.ordinal <= 3000
  ).toSet

  val tzai8000: Set[Grapheme] = allGraphemes.filter(x =>
    x.tzai.isDefined && x.tzai.get.ordinal <= 8000
  ).toSet
  val tzai7000: Set[Grapheme] = allGraphemes.filter(x =>
    x.tzai.isDefined && x.tzai.get.ordinal <= 7000
  ).toSet
  val tzai6000: Set[Grapheme] = allGraphemes.filter(x =>
    x.tzai.isDefined && x.tzai.get.ordinal <= 6000
  ).toSet
  val tzai5000: Set[Grapheme] = allGraphemes.filter(x =>
    x.tzai.isDefined && x.tzai.get.ordinal <= 5000
  ).toSet
  val tzai4000: Set[Grapheme] = allGraphemes.filter(x =>
    x.tzai.isDefined && x.tzai.get.ordinal <= 4000
  ).toSet
  val tzai3000: Set[Grapheme] = allGraphemes.filter(x =>
    x.tzai.isDefined && x.tzai.get.ordinal <= 3000
  ).toSet
}
