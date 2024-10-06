package OutputTranslation

import OutputTranslation._
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import scala.math.Ordered.orderingToOrdered

// Main sorting functions and criteria for Output Entries
object OutputEntryOrdering {

  // Function to sort a set of OutputEntry objects using explicit ordering
  def sortSetOfOutput(input: Set[OutputEntry], primaryCharSystem: CharSystem): List[OutputEntry] = {
    input.toList.sorted(entryOrdering(primaryCharSystem))
  }

  // Obtain the ordering for OutputEntry based on the primary character system
  def entryOrdering(primaryCharSystem: CharSystem): Ordering[OutputEntry] = new Ordering[OutputEntry] {
    override def compare(x: OutputEntry, y: OutputEntry): Int = {
      val xGraphemes: List[String] = Grapheme.splitIntoGraphemes(x.chineseStr)
      val yGraphemes: List[String] = Grapheme.splitIntoGraphemes(y.chineseStr)
      //checkReverseOrderLength(x)
      //checkReverseOrderLength(y)

      // If x should be sorted last and y shouldn't, return 1
      if ((xGraphemes.length == 2) && !(yGraphemes.length == 2)) return 1

      // If y should be sorted last and x shouldn't, return -1
      if (!(xGraphemes.length == 2) && (yGraphemes.length == 2)) return -1

      // Get primary and secondary orders
      val primaryOrderX = getGraphemeOrdering(x, primaryCharSystem).getOrElse(Int.MaxValue)
      val primaryOrderY = getGraphemeOrdering(y, primaryCharSystem).getOrElse(Int.MaxValue)

      val secondaryCharSystem = if (primaryCharSystem == CharSystem.Junda) CharSystem.Tzai else CharSystem.Junda
      val secondaryOrderX = getGraphemeOrdering(x, secondaryCharSystem).getOrElse(Int.MaxValue)
      val secondaryOrderY = getGraphemeOrdering(y, secondaryCharSystem).getOrElse(Int.MaxValue)

      val unicodeOrderX = x.chineseStr.codePoints().toArray.toSeq
      val unicodeOrderY = y.chineseStr.codePoints().toArray.toSeq
      
      // Compare based on primary, secondary, and fall back to unicode
      val primaryComparison = primaryOrderX.compareTo(primaryOrderY)
      if (primaryComparison != 0) return primaryComparison
      
      val secondaryComparison = secondaryOrderX.compareTo(secondaryOrderY)
      if (secondaryComparison != 0) return secondaryComparison

      val unicodeComparison = unicodeOrderX.compare(unicodeOrderY)
      if (unicodeComparison != 0) return unicodeComparison
      
      x.chineseStr.compareTo(y.chineseStr)
    }
  }

  private def checkReverseOrderLength(entry: OutputEntry): Unit = {
    if (entry.jundaReverseOrder.length != entry.tzaiReverseOrder.length) {
      throw new IllegalArgumentException("inpjundaReverseOrder and inptzaiReverseOrder have different lengths")
    }
  }

  // Method to determine if an entry should be sorted last
  private def shouldSortLast(entry: OutputEntry): Boolean = {
    entry.jundaReverseOrder.length == 2 && entry.tzaiReverseOrder.length == 2
  }

  // Retrieve the grapheme ordering based on the char system
  def getGraphemeOrdering(entry: OutputEntry, charSystem: CharSystem): Option[Int] = {
    charSystem match {
      case CharSystem.Junda =>
        entry.jundaReverseOrder.headOption.flatMap(_.junda.map(_.ordinal))
      case CharSystem.Tzai =>
        entry.tzaiReverseOrder.headOption.flatMap(_.tzai.map(_.ordinal))
    }
  }

}

// Extra utilities
// Define ordering for a list of graphemes without implicit dependencies
object GraphemeUtils {
  def listOrdering: Ordering[List[Grapheme]] = new Ordering[List[Grapheme]] {
    def compare(aList: List[Grapheme], bList: List[Grapheme]): Int = {
      (aList, bList) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => 1
        case (aHead :: aTail, bHead :: bTail) =>
          graphemeOrdering.compare(aHead, bHead) match {
            case 0 => compare(aTail, bTail)
            case c => c
          }
      }
    }
  }

  def graphemeOrdering: Ordering[Grapheme] = new Ordering[Grapheme] {
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
}