package OutputTranslation

import OutputTranslation._
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import scala.math.Ordered.orderingToOrdered

object OutputEntryOrdering {
  
  def sortSetOfOutput(input: Set[OutputEntry], primaryCharSystem: CharSystem): List[OutputEntry] = {
    implicit val charSystem: CharSystem = primaryCharSystem // Set the primaryCharSystem as implicit
    input.toList.sorted // Automatically uses the 'entryOrdering' from OutputEntryOrdering
  }
  
  // Helper method to safely handle nulls
  def safeOption[T](value: T): Option[T] = Option(value) // Converts nulls into None

  implicit val entryOrdering: Ordering[OutputEntry] = new Ordering[OutputEntry] {
    override def compare(x: OutputEntry, y: OutputEntry): Int = {
      val primaryCharSystem = CharSystem.Junda
      val secondaryCharSystem = if (primaryCharSystem == CharSystem.Junda) CharSystem.Tzai else CharSystem.Junda

      val primaryOrderX = safeOption(getGraphemeOrdering(x, primaryCharSystem)).getOrElse(Some(Int.MaxValue))
      val primaryOrderY = safeOption(getGraphemeOrdering(y, primaryCharSystem)).getOrElse(Some(Int.MaxValue))

      val secondaryOrderX = safeOption(getGraphemeOrdering(x, secondaryCharSystem)).getOrElse(Some(Int.MaxValue))
      val secondaryOrderY = safeOption(getGraphemeOrdering(y, secondaryCharSystem)).getOrElse(Some(Int.MaxValue))

      // Extract Unicode points
      val unicodeOrderX = x.chineseStr.codePoints().toArray.toSeq
      val unicodeOrderY = y.chineseStr.codePoints().toArray.toSeq

      // Compare based on primary, secondary, and fall back to unicode
      val primaryComparison = implicitly[Ordering[Option[Int]]].compare(primaryOrderX, primaryOrderY)
      if (primaryComparison != 0) return primaryComparison

      val secondaryComparison = implicitly[Ordering[Option[Int]]].compare(secondaryOrderX, secondaryOrderY)
      if (secondaryComparison != 0) return secondaryComparison

      // Compare Unicode sequences when primary and secondary orders are equal
      val unicodeComparison = unicodeOrderX.compare(unicodeOrderY)
      if (unicodeComparison != 0) return unicodeComparison

      // Final fallback: compare string representations of `chineseStr`
      x.chineseStr.compareTo(y.chineseStr)
    }
  }

  def getGraphemeOrdering(entry: OutputEntry, charSystem: CharSystem): Option[Int] = {
    val order = charSystem match {
      case CharSystem.Junda =>
        Option(entry.jundaReverseOrder).flatMap(_.headOption.flatMap(_.junda.map(_.ordinal)))
      case CharSystem.Tzai =>
        Option(entry.tzaiReverseOrder).flatMap(_.headOption.flatMap(_.tzai.map(_.ordinal)))
    }
    order.orElse(Some(Int.MaxValue)) // Provide a safe default if ordering is not available
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

  implicit val seqIntOrdering: Ordering[Seq[Int]] = new Ordering[Seq[Int]] {
    def compare(x: Seq[Int], y: Seq[Int]): Int = {
      val lengthCompare = x.length.compare(y.length)
      if (lengthCompare != 0) {
        lengthCompare
      } else {
        x.zip(y).foldLeft(0) {
          case (acc, (a, b)) if acc == 0 => a.compare(b)
          case (acc, _) => acc
        }
      }
    }
  }

  implicit val tupleOrdering: Ordering[(Option[Int], List[Grapheme], Option[Int], String)] = {
    val highOptionIntOrdering: Ordering[Option[Int]] = Ordering.Option(Ordering.by((i: Int) => -i))
    Ordering.Tuple4(highOptionIntOrdering, listOrdering, highOptionIntOrdering, Ordering.String)
  }
}