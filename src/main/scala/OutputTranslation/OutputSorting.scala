package OutputTranslation

import UtilityClasses.CharSystem.{Junda, Tzai}
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.SpecialCharacters.ReadSpecialCharacters
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.{SortedMap, mutable}

class OutputSorting {
  def sortOutputEntries(): mutable.SortedMap[String, Int] = {
    implicit val customOrdering: Ordering[String] = new Ordering[String] {
      def compare(x: String, y: String): Int = {
        val lengthCompare = x.length.compare(y.length)
        if (lengthCompare != 0) lengthCompare else x.compare(y)
      }
    }

    // Create a SortedMap using the custom ordering
    val sortedMap: mutable.SortedMap[String, Int] = mutable.SortedMap(
      "apple" -> 1,
      "banana" -> 2,
      "pear" -> 3,
      "fig" -> 4,
      "kiwi" -> 5,
      "blueberry" -> 6
    )
    sortedMap
  }


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

  def mapFromOutput(input: List[Set[OutputEntry]], charSystem: CharSystem): SortedMap[String, List[OutputEntry]] = {
    var res: mutable.SortedMap[String, List[OutputEntry]] = mutable.SortedMap[String, List[OutputEntry]]()

    val merge: Set[OutputEntry] = mergeOutputEntries(input)
    val codeToOutput:  mutable.SortedMap[String, Set[OutputEntry]] = codeToOutputEntry(merge)
    for ((myKey, myVal) <- codeToOutput) {
      val sortedVal: List[OutputEntry] = sortSetOfOutput(myVal, charSystem)
      res.addOne((myKey, sortedVal))
    }
    return SortedMap.from(res)
  }

  def sortSetOfOutput(input: Set[OutputEntry], charSystem: CharSystem): List[OutputEntry] = {
    implicit val entryOrdering: Ordering[OutputEntry] = new Ordering[OutputEntry] {
      override def compare(a: OutputEntry, b: OutputEntry): Int = {
        val aOrdering = getGraphemeOrdering(a, charSystem)
        val bOrdering = getGraphemeOrdering(b, charSystem)

        (aOrdering, bOrdering) match {
          case (Some(aOrd), Some(bOrd)) =>
            val initialCompare = aOrd.compareTo(bOrd)
            if (initialCompare != 0) {
              initialCompare
            } else {
              compareNextGrapheme(a.jundaReverseOrder, b.jundaReverseOrder, charSystem)
            }
          case (Some(_), None) => -1 // Entry `a` should come before Entry `b`
          case (None, Some(_)) => 1 // Entry `b` should come before Entry `a`
          case (None, None) => 0 // Both have no grapheme ordering data available
        }
      }
    }

    input.toList.sorted
  }

  def getGraphemeOrdering(entry: OutputEntry, charSystem: CharSystem): Option[Int] = {
    charSystem match {
      case CharSystem.Junda =>
        entry.jundaReverseOrder.headOption.flatMap(_.junda.map(_.ordinal))
      case CharSystem.Tzai =>
        entry.tzaiReverseOrder.headOption.flatMap(_.tzai.map(_.ordinal))
    }
  }

  def compareNextGrapheme(aList: List[Grapheme], bList: List[Grapheme], charSystem: CharSystem): Int = {
    def orderingFunc(grapheme: Grapheme): Option[Int] = {
      charSystem match {
        case CharSystem.Junda => grapheme.junda.map(_.ordinal)
        case CharSystem.Tzai => grapheme.tzai.map(_.ordinal)
      }
    }

    (aList.tail.headOption.flatMap(orderingFunc), bList.tail.headOption.flatMap(orderingFunc)) match {
      case (Some(aOrdNext), Some(bOrdNext)) =>
        val nextCompare = aOrdNext.compareTo(bOrdNext)
        if (nextCompare != 0) nextCompare
        else compareNextGrapheme(aList.tail, bList.tail, charSystem)
      case (Some(_), None) => -1 // `a` has more graphemes remaining
      case (None, Some(_)) => 1 // `b` has more graphemes remaining
      case (None, None) => 0 // Both lists are exhausted
    }
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

        val outputNewEntry = new OutputEntry(
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
  
  /*
  def mergeOutputEntries(input: List[Set[OutputEntry]]): Set[OutputEntry] = {
    var res: mutable.Map[String, Set[OutputEntry]] = mutable.Map[String, Set[OutputEntry]]()
    for (collOfEntry <- input) {
      for (entry <- collOfEntry) {
        if (entry.chineseStr == "七") {
          val test = ""
        }
        if (!res.contains(entry.chineseStr)) {
          res.addOne((entry.chineseStr, Set(entry)))
        } else {
          
          val updatedvalue: Set[OutputEntry] = res.get(entry.chineseStr).get ++ Set(entry)
          res.update(entry.chineseStr, updatedvalue)
        }
      }
    }
    var finalRes: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for ((mykey, eachSet) <- res) {
      if (eachSet.size == 1) {
        finalRes.addOne(eachSet.toList(0))
      } else{
        //val chineseStr: String = inputChineseStr
        //  val meaning: String = inputMeaning
        //  val pron: String = inputPronounciation
        //  val tradSimp: String = inputTradSimp
        //  val jundaReverseOrder: List[Grapheme] = inpjundaReverseOrder
        //  val tzaiReverseOrder: List[Grapheme] = inptzaiReverseOrder
        //  val codes: Set[String] = inpcodes
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
        val outputNewEntry: OutputEntry = OutputEntry(mykey, updatedMeaning, updatedPron, updatedTradSimp,
          updatedJundaReverseOrder, updatedTzaiReverseOrder, updatedCodes)
        finalRes.addOne(outputNewEntry)
      }
    }
    finalRes.toSet
  }*/

}

object OutputSorting {
  val outClass = new OutputTranslation()
  val outSorting = new OutputSorting()
  val cedictSet: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val cedictSetOut: Set[OutputEntry] = OutputTranslation.outputCedict
  val conFull: Set[OutputEntry] = OutputTranslation.conwayOutFull
  val specialChars: Set[OutputEntry] = ReadSpecialCharacters.allCharacterOutput.toSet
  val mapFullJunda: SortedMap[String, List[OutputEntry]] = outSorting.mapFromOutput(List(specialChars, conFull, cedictSetOut), Junda)
  val mapFullTzai: SortedMap[String, List[OutputEntry]] = outSorting.mapFromOutput(List(specialChars, conFull, cedictSetOut), Tzai)
  
  //val conway: Set[OutputEntry] =  OutputTranslation.outputConway//GenerateConwayCodes.conwaySet
}
