package OutputTranslation

import ElementGenerator.{ElementList, ElementType}
import Sorting.OutputEntryOrdering
import UtilityClasses.CharSystem.{Junda, NotHanChar, Tzai}
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.SpecialCharacters.ReadSpecialCharacters
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.math.Ordering.Implicits.*
import scala.jdk.StreamConverters.*
import UtilityClasses.{CharSystem, OutputEntry}
import Sorting.OutputEntryOrdering.*
import staticFileGenerators.JundaFrequency.GenerateJundaMap
import staticFileGenerators.TzaiFrequency.GenerateTzaiMap

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import staticFileGenerators.JundaFrequency.JundaData
import staticFileGenerators.TzaiFrequency.TzaiData

import scala.collection.mutable.HashMap

class OutputSorting {

  // Method to provide custom ordering
  def customOrdering: Ordering[String] = new Ordering[String] {
    override def compare(x: String, y: String): Int = {
      val lengthCompare = x.length.compare(y.length)
      if (lengthCompare != 0) lengthCompare else x.compare(y)
    }
  }

  def codeToOutputEntry(input: Set[OutputEntry]): mutable.SortedMap[String, Set[OutputEntry]] = {
    implicit val ordering: Ordering[String] = customOrdering
    val sortedMap: mutable.SortedMap[String, Set[OutputEntry]] = mutable.SortedMap[String, Set[OutputEntry]]()

    for (eachEntry <- input) {
      for (code <- eachEntry.codes) {
        val updatedSet = sortedMap.getOrElse(code, Set()) + eachEntry
        sortedMap.update(code, updatedSet)
      }
    }
    sortedMap
  }

  def mapFromOutput(input: List[List[OutputEntry]], charSystem: CharSystem): SortedMap[String, List[OutputEntry]] = {
    implicit val ordering: Ordering[String] = customOrdering
    var res: mutable.SortedMap[String, List[OutputEntry]] = mutable.SortedMap[String, List[OutputEntry]]()

    val merge: Set[OutputEntry] = mergeOutputEntries(input)
    val codeToOutput: mutable.SortedMap[String, Set[OutputEntry]] = codeToOutputEntry(merge)

    val test = ""
    for ((myKey, myVal) <- codeToOutput) {
      val sortedVal: List[OutputEntry] = OutputEntryOrdering.sortSetOfOutput(myVal, charSystem)
      if (sortedVal.size > 10) {
        val test = ""
      }
      res.addOne((myKey, sortedVal))
    }
    SortedMap.from(res)(ordering)
  }

  def mergeOutputEntries(input: List[List[OutputEntry]]): Set[OutputEntry] = {
    val res: mutable.Map[String, List[OutputEntry]] = mutable.Map()

    // Merge entries by their `chineseStr`
    for (collOfEntry <- input; entry <- collOfEntry) {
      res.updateWith(entry.chineseStr) {
        case Some(existingEntries) => Some(existingEntries :+ entry)
        case None => Some(List(entry))
      }
    }

    val finalRes: mutable.Set[OutputEntry] = mutable.Set()
    // Create new merged entries based on the grouped values
    for ((chineseStr, eachList) <- res) {
      if (eachList.size == 1) {
        finalRes.add(eachList.head)
      } else {
        var updatedMeaning = ""
        var updatedPron = ""
        var updatedTradSimp = ""
        var updatedJundaReverseOrder: List[Grapheme] = List()
        var updatedTzaiReverseOrder: List[Grapheme] = List()
        var updatedCodes: Set[String] = Set()

        for (ent <- eachList) {
          if (updatedMeaning.isEmpty || ent.meaning.nonEmpty) {
            updatedMeaning = ent.meaning
          }
          if (updatedPron.isEmpty || ent.pron.nonEmpty) {
            updatedPron = ent.pron
          }
          if (updatedTradSimp.isEmpty || ent.tradSimp.nonEmpty) {
            updatedTradSimp = ent.tradSimp
          }
          if (updatedJundaReverseOrder.isEmpty || ent.jundaReverseOrder.nonEmpty) {
            updatedJundaReverseOrder = ent.jundaReverseOrderG
          }
          if (updatedTzaiReverseOrder.isEmpty || ent.tzaiReverseOrder.nonEmpty) {
            updatedTzaiReverseOrder = ent.tzaiReverseOrderG
          }
          updatedCodes = updatedCodes ++ ent.codes
        }

        val outputNewEntry = OutputEntry(
          chineseStr,
          updatedMeaning,
          updatedPron,
          updatedTradSimp,
          updatedJundaReverseOrder,
          updatedTzaiReverseOrder,
          updatedCodes
        )
        finalRes.add(outputNewEntry)
      }
    }
    finalRes.toSet
  }

  def getStringsFromElements(elems: Set[ElementType]): Set[String] = {
    val res: Set[String] = elems.map(x => x.rawString)
    if (res != null && res.nonEmpty) {
      return res
    } else {
      throw new Exception("No elements found")
    }
  }

  //val simpNotFoundInTrad:  HashMap[String, JundaData] = jundaFreqMap.filter(x => !allTraditional.contains(x._1))
  def generateSystemSet(jundaFreqMap: HashMap[String, JundaData],
                        cedictSimp: Set[CedictEntry],
                        tzaiFreqMap: HashMap[String, TzaiData] ,
                        cedictTrad: Set[CedictEntry],
                        conwaySet: Set[Grapheme] ,
                        charSystem: CharSystem): Set[String] = {
    val testTrad: Set[String] = createStringOfAllWordsAndCharacters(cedictTrad)
    val allTraditional: Set[String] = testTrad ++ tzaiFreqMap.map(x => x._1).toSet

    val testSimp: Set[String] = createStringOfAllWordsAndCharacters(cedictSimp)
    val simpFreq: Set[String] = jundaFreqMap.map(x => x._1).toSet
    val simpFreqMinusTrad: Set[String] = simpFreq.filter(x => !allTraditional.contains(x))
    val allSimplified: Set[String] = testSimp ++ simpFreqMinusTrad

    //val missingFromBoth: Set[String] = simpFreq.filter(x => !allSimplified.contains(x) && !allTraditional.contains(x))

    val conwayStrSet: Set[String] = conwaySet.map(x => x.char)

    val traditionalNonHan: Set[String] = allTraditional.filter(x => x.codePoints()
      .mapToObj(cp => new String(Character.toChars(cp))).allMatch(c => !conwayStrSet.contains(c)))

    val simplifiedNonHan: Set[String] = allSimplified.filter(x => x.codePoints()
      .mapToObj(cp => new String(Character.toChars(cp))).allMatch(c => !conwayStrSet.contains(c)))

    val allTraditional_noOnlyAscii: Set[String] = allTraditional.filter(x => !traditionalNonHan.contains(x))
    val allSimplified_noOnlyAscii: Set[String] = allSimplified.filter(x => !simplifiedNonHan.contains(x))

    //add characters from conway that are not in cedict, junda or tzai
    val conwayStr: Set[String] = OutputSorting.conwaySet.map(x => x.char).toSet
    val combinedSimpAndTrad: Set[String] = allTraditional_noOnlyAscii ++ allSimplified_noOnlyAscii
    val conwayMissingFromCombined: Set[String] = conwayStr.filter(x => !combinedSimpAndTrad.contains(x))


    if (CharSystem.Tzai == charSystem) {
      return allTraditional_noOnlyAscii ++ conwayMissingFromCombined
    }
    if (CharSystem.Junda == charSystem) {
      return allSimplified_noOnlyAscii
    }
    if (CharSystem.NotHanChar == charSystem) {
      return traditionalNonHan ++ simplifiedNonHan
    }

    throw Exception("charSystem not found")
  }

  private def createStringOfAllWordsAndCharacters(cedict: Set[CedictEntry]): Set[String] = {
    val basicset: Set[String] = cedict.view.map(_.chineseStr).toSet
    val singles: Set[String] = basicset.flatMap(_.codePoints().mapToObj(cp => new String(Character.toChars(cp))).toScala(Seq)).toSet
    return basicset ++ singles
  }


}


object OutputSorting {
  val outClass = new OutputTranslation()
  val outSorting = new OutputSorting()
  
  // Compute Junda lazily to break init cycles with Grapheme/other singletons
  lazy val jundaMap: HashMap[String, JundaData] = GenerateJundaMap.mapJundaData
  // Compute Tzai lazily to break init cycles with Grapheme/other singletons
  lazy val tzaiMap: HashMap[String, TzaiData] = GenerateTzaiMap.mapTzaiData

  val conwaySet: Set[Grapheme] = GenerateConwayCodes.conwaySet
  val elements: Set[String] = outSorting.getStringsFromElements(ElementList.elementTypes)
  val cedictSet: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val sortingCedictSimpSet: Set[CedictEntry] = GenerateCedictMap.cedictSimpSet
  val sortingCedictTradSet: Set[CedictEntry] = GenerateCedictMap.cedictTradSet
  val cedictSetOut: Set[OutputEntry] = OutputTranslation.outputCedict
  val conFull: Set[OutputEntry] = OutputTranslation.outputConway //OutputTranslation.conwayOutFull
  val specialChars: List[OutputEntry] = ReadSpecialCharacters.allCharacterOutput
  val allSimplified: Set[String] = outSorting.generateSystemSet(jundaMap, sortingCedictSimpSet, tzaiMap, sortingCedictTradSet, conwaySet, Junda)
  val allTraditional: Set[String] = outSorting.generateSystemSet(jundaMap, sortingCedictSimpSet, tzaiMap, sortingCedictTradSet, conwaySet, Tzai)
  val allNonHan: Set[String] = outSorting.generateSystemSet(jundaMap, sortingCedictSimpSet, tzaiMap, sortingCedictTradSet, conwaySet, NotHanChar)

  //val allTraditional: Set[String] =
  val mapFullJunda: SortedMap[String, List[OutputEntry]] = outSorting.mapFromOutput(
    List(conFull.toList, cedictSetOut.toList), Junda)
  val mapFullTzai: SortedMap[String, List[OutputEntry]] = outSorting.mapFromOutput(
    List(conFull.toList, cedictSetOut.toList), Tzai)
  //yveo 449 称 449, 颓 2996, 頺 8820, 稧 8851, 秼 9416, 龝 9710, 稴 9748, 頽 2147483647, 棃 2147483647, 穕 2147483647

  val test = mapFullTzai.get("yveo")
  val test2 = mapFullTzai.get("aroz")
  val tes2 = ""
}
















