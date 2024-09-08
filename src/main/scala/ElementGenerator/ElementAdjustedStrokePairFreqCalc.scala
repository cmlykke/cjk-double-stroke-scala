package ElementGenerator

import ElementGenerator.ElementAdjustedStrokePairFreqCalc.{elemAdjustedJunda, elemAdjustedTzai}
import UtilityClasses.{CharSystem, ConwayUnambigous, StaticFileCharInfo, StaticFileCharInfoWithLetterConway}
import staticFileGenerators.JundaFrequency.JundaData
import staticFileGenerators.TzaiFrequency.TzaiData

import scala.collection.mutable

class ElementAdjustedStrokePairFreqCalc {

  def FourCodeFreqMap(system: CharSystem): Map[String, Double] = {
    system match
      case CharSystem.Junda => generateJundaFourCodeMap()
      case CharSystem.Tzai => generateTzaiFourCodeMap()
  }

  private def generateJundaFourCodeMap(): Map[String, Double] = {
    // Corrected mutable map initialization
    val res: mutable.Map[String, Double] = mutable.Map()
    elemAdjustedJunda.foreach { elem =>
      val mapForEachElem: mutable.Map[String, Double] = generateJudaMapForSingle(elem)
      mergeMaps(res, mapForEachElem)
    }
    res.toMap // Convert mutable map to immutable before returning
  }

  private def generateTzaiFourCodeMap(): Map[String, Double] = {
    // Corrected mutable map initialization
    val res: mutable.Map[String, Double] = mutable.Map()
    elemAdjustedTzai.foreach { elem =>
      val mapForEachElem: mutable.Map[String, Double] = generateTzaiMapForSingle(elem)
      mergeMaps(res, mapForEachElem)
    }
    res.toMap // Convert mutable map to immutable before returning
  }

  private def generateTzaiMapForSingle(input: StaticFileCharInfoWithLetterConway): mutable.Map[String, Double] = {
    // Initialize the result map with mutable.Map
    val res: mutable.Map[String, Double] = mutable.Map()

    // Extract the frequency from the input
    val freqOption: Option[TzaiData] = input.grapheme.tzai
    if (freqOption.isDefined) {
      // Filter for ConwayUnambigous with 4 or fewer conwayPairs
      val fourOrLess: Set[ConwayUnambigous] = input.letterConway
        .filter(_.conwayPairs.size <= 4)
      val divDouble = freqOption.get.frequency / fourOrLess.size

      // Update the result map
      fourOrLess.foreach { conway =>
        conway.conwayPairs.foreach { pair =>
          res(pair) = res.getOrElse(pair, 0.0) + divDouble
        }
      }
    }
    res
  }

  private def generateJudaMapForSingle(input: StaticFileCharInfoWithLetterConway): mutable.Map[String, Double] = {
    // Initialize the result map with mutable.Map
    val res: mutable.Map[String, Double] = mutable.Map()

    // Extract the frequency from the input
    val freqOption: Option[JundaData] = input.grapheme.junda
    if (freqOption.isDefined) {
      // Filter for ConwayUnambigous with 4 or fewer conwayPairs
      val fourOrLess: Set[ConwayUnambigous] = input.letterConway
        .filter(_.conwayPairs.size <= 4)
      val divDouble = freqOption.get.frequency / fourOrLess.size

      // Update the result map
      fourOrLess.foreach { conway =>
        conway.conwayPairs.foreach { pair =>
          res(pair) = res.getOrElse(pair, 0.0) + divDouble
        }
      }
    }
    res
  }

  private def mergeMaps(nr1: mutable.Map[String, Double], nr2: mutable.Map[String, Double]): Unit = {
    nr2.foreach { case (key, value) =>
      if (nr1.contains(key)) {
        nr1(key) += value
      } else {
        nr1(key) = value
      }
    }
  }
}

object ElementAdjustedStrokePairFreqCalc {

  val elemAdjustedJunda: Set[StaticFileCharInfoWithLetterConway] =
    ElementAdjustedCodes.elemAdjusted8000Junda
  val elemAdjustedTzai: Set[StaticFileCharInfoWithLetterConway] =
    ElementAdjustedCodes.elemAdjusted8000Tzai

  // val overlapJUNDA = ElementAdjustedCodes.secondOverlapJunda
  // val jundaUPDATEDformatted = formatFinalRes(overlapJUNDA, CharSystem.Junda)

  // val overlapTzai = ElementAdjustedCodes.secondOverlapTzai
  // val tzaiUPDATEDformatted = formatFinalRes(overlapTzai, CharSystem.Tzai)
}