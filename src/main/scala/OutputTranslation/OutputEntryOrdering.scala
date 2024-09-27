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
      // Get primary and secondary orders
      val primaryOrderX = getGraphemeOrdering(x, primaryCharSystem).getOrElse(Int.MaxValue)
      val primaryOrderY = getGraphemeOrdering(y, primaryCharSystem).getOrElse(Int.MaxValue)

      val secondaryCharSystem = if (primaryCharSystem == CharSystem.Junda) CharSystem.Tzai else CharSystem.Junda
      val secondaryOrderX = getGraphemeOrdering(x, secondaryCharSystem).getOrElse(Int.MaxValue)
      val secondaryOrderY = getGraphemeOrdering(y, secondaryCharSystem).getOrElse(Int.MaxValue)

      // Extract Unicode points
      val unicodeOrderX = x.chineseStr.codePoints().toArray.toSeq
      val unicodeOrderY = y.chineseStr.codePoints().toArray.toSeq

      // Compare based on primary, secondary, and fall back to unicode
      val primaryComparison = primaryOrderX.compareTo(primaryOrderY)
      if (primaryComparison != 0) return primaryComparison

      val secondaryComparison = secondaryOrderX.compareTo(secondaryOrderY)
      if (secondaryComparison != 0) return secondaryComparison

      // Compare Unicode sequences when primary and secondary orders are equal
      val unicodeComparison = unicodeOrderX.compare(unicodeOrderY)
      if (unicodeComparison != 0) return unicodeComparison

      // Final fallback: compare string representations of `chineseStr`
      x.chineseStr.compareTo(y.chineseStr)
    }
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