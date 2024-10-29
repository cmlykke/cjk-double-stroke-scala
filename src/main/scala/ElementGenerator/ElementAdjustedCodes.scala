package ElementGenerator

import OverlapCalc.OverlapCalculations
import OverlapCalc.OverlapCalculations.staticfile
import UtilityClasses.{CharSystem, ConwayColl, ConwayUnambigous, Grapheme, InputSizes, StaticFileCharInfo, StaticFileCharInfoWithLetterConway}
import staticFileGenerators.Conway.ReadConwayData
import staticFileGenerators.StaticFileGeneratorFacade

import scala.collection.mutable
import scala.collection.mutable.HashMap

class ElementAdjustedCodes {

  // Generates element adjusted codes for a given character system
  

  // Generates the overlap map for a given character system
  def generateOverlapMap(charSystem: CharSystem, decorated: Set[StaticFileCharInfoWithLetterConway]):
    Map[ConwayUnambigous, Set[StaticFileCharInfoWithLetterConway]] = {
    val result1: OverlapCalculations = new OverlapCalculations
    val result1b: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]] =
      result1.calculateOverlapDecorated(decorated, charSystem, ElementAdjustedCodes.inpSizes)

    // Convert the mutable map with mutable sets to an immutable map with immutable sets
    result1b.map { case (key, mutableSet) =>
      key -> mutableSet.toSet
    }.toMap
  }

  // Generates the second overlap map for a given character system
  def secondOverlapMap(charSystem: CharSystem, decorated: Set[StaticFileCharInfoWithLetterConway]):
    List[(Int, List[StaticFileCharInfoWithLetterConway])] = {
    val result1: OverlapCalculations = new OverlapCalculations
    val result1b: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfoWithLetterConway]] =
      result1.calculateOverlapDecorated(decorated, charSystem, ElementAdjustedCodes.inpSizes)

    val finalres: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
      result1.getOverlapDecorated(charSystem, result1b)
    finalres
  }

}

object ElementAdjustedCodes {

  val inpSizes: InputSizes = InputSizes.Three_oneAndFive_one
  val jundaChars: Set[Grapheme] = OverlapCalculations.junda8000
  val tzaiChars: Set[Grapheme] = OverlapCalculations.tzai8000
  //val allChars: Set[Grapheme] = OverlapCalculations.allGraphemes
  val adjusted: ElementAdjustedCodes = new ElementAdjustedCodes()

  // Generating element adjusted codes
  val elemAdjusted8000Junda: Set[StaticFileCharInfoWithLetterConway] = ElementAdjustedCodesNrTwo.generateElementAdjustedCodes(jundaChars)
  val elemAdjusted8000Tzai: Set[StaticFileCharInfoWithLetterConway] = ElementAdjustedCodesNrTwo.generateElementAdjustedCodes(tzaiChars)
  val elemAdjustedAllChars: Set[StaticFileCharInfoWithLetterConway] = ElementAdjustedCodesNrTwo.generateElementAdjustedCodes(OverlapCalculations.allGraphemes)
  // generate overlap maps
  

  // Generating second overlap maps
  val secondOverlapJunda: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
    adjusted.secondOverlapMap(CharSystem.Junda, elemAdjusted8000Junda)
  val secondOverlapTzai: List[(Int, List[StaticFileCharInfoWithLetterConway])] =
    adjusted.secondOverlapMap(CharSystem.Tzai, elemAdjusted8000Tzai)
  
}

object ElementAdjustedCodesNrTwo {
  /*
  def generateElementAdjustedCodes(graphemes: Set[Grapheme]): Set[StaticFileCharInfoWithLetterConway] = {
    try {
      val charInfoSet: Set[StaticFileCharInfo] = StaticFileGeneratorFacade.getAll(graphemes)
      val elements = ElementList.elementTypes
      val decoratedList: Set[StaticFileCharInfoWithLetterConway] =
        new ElementEditor().createDecoratedCharInfo(charInfoSet, elements)
      return decoratedList
    } catch {
      case e: Exception => throw Exception("adjusted code error")//e.printStackTrace() // Handle the exception appropriately
    }
  }*/

  def generateElementAdjustedCodeSingle(grapheme: Grapheme): Option[StaticFileCharInfoWithLetterConway] = {
    try {
      val charInfoSet: Set[StaticFileCharInfo] = StaticFileGeneratorFacade.getAll(Set(grapheme))
      val elements = ElementList.elementTypes
      // Assuming createDecoratedCharInfo returns a collection of StaticFileCharInfoWithLetterConway
      val res = new ElementEditor().createDecoratedCharInfo(charInfoSet, elements).head
      Some(res)
    } catch {
      case _: Exception => None // Specific or general exception can be caught
    }
  }

  // Main function that takes a Set[Grapheme]
  def generateElementAdjustedCodes(graphemes: Set[Grapheme]): Set[StaticFileCharInfoWithLetterConway] = {
    var res: mutable.Set[StaticFileCharInfoWithLetterConway] = mutable.Set()
    for (graph <- graphemes) {
      val opt: Option[StaticFileCharInfoWithLetterConway] = 
        generateElementAdjustedCodeSingle(graph)
      if (opt.isDefined) {
        res.add(opt.get)
      } else {
        throw Exception("adjusted code error")
      }
    }
    return res.toSet
  }
  
  
}
