package OutputTranslation

import UtilityClasses.CharSystem.{Junda, Tzai}
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.SpecialCharacters.ReadSpecialCharacters
import staticFileGenerators.cedictMap.GenerateCedictMap
import scala.math.Ordering.Implicits._
import scala.jdk.StreamConverters._

import scala.collection.{SortedMap, mutable}

class OutputSorting {
  
  def codeToOutputEntry(input: Set[OutputEntry]): mutable.SortedMap[String, Set[OutputEntry]] = {
    implicit val customOrdering: Ordering[String] = new Ordering[String] {
      def compare(x: String, y: String): Int = {
        val lengthCompare = x.length.compare(y.length)
        if (lengthCompare != 0) lengthCompare else x.compare(y)
      }
    }
    val sortedMap: mutable.SortedMap[String, Set[OutputEntry]] = mutable.SortedMap[String, Set[OutputEntry]]()

    for (eachEntry <- input) {
      for (code <- eachEntry.codes) {
        val updatedSet = sortedMap.getOrElse(code, Set()) + eachEntry
        sortedMap.update(code, updatedSet)
      }
    }
    sortedMap
  }

  def mapFromOutput(input: List[Set[OutputEntry]],
                    charSystem: CharSystem): SortedMap[String, List[OutputEntry]] = {
    var res: mutable.SortedMap[String, List[OutputEntry]] = mutable.SortedMap[String, List[OutputEntry]]()

    val merge: Set[OutputEntry] = mergeOutputEntries(input)
    val codeToOutput:  mutable.SortedMap[String, Set[OutputEntry]] = codeToOutputEntry(merge)
    for ((myKey, myVal) <- codeToOutput) {
      val sortedVal: List[OutputEntry] = sortSetOfOutput(myVal, charSystem)
      res.addOne((myKey, sortedVal))
    }
    return SortedMap.from(res)
  }

  /////////////////////////////////////////
  
  def sortSetOfOutput(input: Set[OutputEntry], primaryCharSystem: CharSystem): List[OutputEntry] = {
    val secondaryCharSystem = if (primaryCharSystem == CharSystem.Junda) CharSystem.Tzai else CharSystem.Junda

    implicit val entryOrdering: Ordering[OutputEntry] = Ordering.by { (entry: OutputEntry) =>
      (
        getGraphemeOrdering(entry, primaryCharSystem),
        //getReverseOrder(entry, primaryCharSystem), // Adjusting to dynamically get the reverse order based on primaryCharSystem
        getGraphemeOrdering(entry, secondaryCharSystem),
        //getReverseOrder(entry, secondaryCharSystem), // Secondary reverse order for comparison
        entry.chineseStr.codePoints().toArray.toSeq
      )
    }
    input.toList.sorted
  }

  def getGraphemeOrdering(entry: OutputEntry, charSystem: CharSystem): Option[Int] = {
    charSystem match {
      case CharSystem.Junda => entry.jundaReverseOrder.headOption
        .flatMap(_.junda.map(_.ordinal)).orElse(Some(Int.MaxValue))
      case CharSystem.Tzai => entry.tzaiReverseOrder.headOption
        .flatMap(_.tzai.map(_.ordinal)).orElse(Some(Int.MaxValue))
    }
  }

  def getReverseOrder(entry: OutputEntry, charSystem: CharSystem): List[Grapheme] = {
    charSystem match {
      case CharSystem.Junda => entry.jundaReverseOrder
      case CharSystem.Tzai => entry.tzaiReverseOrder
    }
  }

  implicit val graphemeOrdering: Ordering[Grapheme] = new Ordering[Grapheme] {
    def compare(g1: Grapheme, g2: Grapheme): Int = {
      (g1.junda, g2.junda, g1.tzai, g2.tzai) match {
        case (Some(j1), Some(j2), _, _) => j1.ordinal.compare(j2.ordinal)
        case (None, Some(_), _, _) => 1
        case (Some(_), None, _, _) => -1
        case (_, _, Some(t1), Some(t2)) => t1.ordinal.compare(t2.ordinal)
        case (_, _, None, Some(_)) => 1
        case (_, _, Some(_), None) => -1
        case _ => 0
      }
    }
  }

  implicit def listOrdering(implicit ord: Ordering[Grapheme]): Ordering[List[Grapheme]] = new Ordering[List[Grapheme]] {
    def compare(aList: List[Grapheme], bList: List[Grapheme]): Int = {
      (aList, bList) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => 1
        case (aHead :: aTail, bHead :: bTail) =>
          ord.compare(aHead, bHead) match {
            case 0 => compare(aTail, bTail)
            case c => c
          }
      }
    }
  }

  implicit val tupleOrdering: Ordering[(Option[Int], List[Grapheme], Option[Int], String)] = {
    val highOptionIntOrdering: Ordering[Option[Int]] = Ordering.Option(Ordering.by((i: Int) => -i))
    Ordering.Tuple4(highOptionIntOrdering, listOrdering, highOptionIntOrdering, Ordering.String)
  }

  def mergeOutputEntries(input: List[Set[OutputEntry]]): Set[OutputEntry] = {
    val res: mutable.Map[String, Set[OutputEntry]] = mutable.Map()

    // Merge entries by their `chineseStr`
    for (collOfEntry <- input; entry <- collOfEntry) {
      res.updateWith(entry.chineseStr) {
        case Some(existingEntries) => Some(existingEntries + entry)
        case None => Some(Set(entry))
      }
    }

    val finalRes: mutable.Set[OutputEntry] = mutable.Set()

    // Create new merged entries based on the grouped values
    for ((chineseStr, eachSet) <- res) {
      if (eachSet.size == 1) {
        finalRes.add(eachSet.head)
      } else {
        var updatedMeaning = ""
        var updatedPron = ""
        var updatedTradSimp = ""
        var updatedJundaReverseOrder: List[Grapheme] = List()
        var updatedTzaiReverseOrder: List[Grapheme] = List()
        var updatedCodes: Set[String] = Set()

        for (ent <- eachSet) {
          if (updatedMeaning.size < ent.meaning.size) {
            updatedMeaning = ent.meaning
          }
          if (updatedPron.size < ent.pron.size) {
            updatedPron = ent.pron
          }
          if (updatedTradSimp.size < ent.tradSimp.size) {
            updatedTradSimp = ent.tradSimp
          }
          if (updatedJundaReverseOrder.size < ent.jundaReverseOrder.size) {
            updatedJundaReverseOrder = ent.jundaReverseOrder
          }
          if (updatedTzaiReverseOrder.size < ent.tzaiReverseOrder.size) {
            updatedTzaiReverseOrder = ent.tzaiReverseOrder
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
}

object OutputSorting {
  val outClass = new OutputTranslation()
  val outSorting = new OutputSorting()
  val cedictSet: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val cedictSetOut: Set[OutputEntry] = OutputTranslation.outputCedict
  val conFull: Set[OutputEntry] = OutputTranslation.conwayOutFull
  val specialChars: Set[OutputEntry] = ReadSpecialCharacters.allCharacterOutput.toSet
  val mapFullJunda: SortedMap[String, List[OutputEntry]] = outSorting.mapFromOutput(
    List(conFull, cedictSetOut), Junda)
  val mapFullTzai: SortedMap[String, List[OutputEntry]] = outSorting.mapFromOutput(
    List(conFull, cedictSetOut), Tzai)
}
