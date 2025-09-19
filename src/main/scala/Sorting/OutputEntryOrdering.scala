package Sorting

import OutputTranslation.*
import UtilityClasses.CharSystem.{Junda, Tzai}
import UtilityClasses.{CedictEntry, CharSystem, Grapheme, OutputEntry}
import staticFileGenerators.Academiasinica.{GenerateSinicaMap, SinicaData}
import staticFileGenerators.BLCUmap.{BLCUData, GenerateBLCUmap}
import staticFileGenerators.JundaFrequency.JundaData


import scala.math.Ordered.orderingToOrdered

object OutputEntryOrdering {

  val simplifiedCedict: Set[String] = OutputSorting.sortingCedictSimpSet.map(x => x.chineseStr)
  val traditionalCedict: Set[String] = OutputSorting.sortingCedictTradSet.map(x => x.chineseStr)

  def sortSetOfOutput(input: Set[OutputEntry], primaryCharSystem: CharSystem): List[OutputEntry] = {
    input.toList.sorted(entryOrdering(primaryCharSystem))
  }

  // Obtain the ordering for OutputEntry based on the primary character system
  def entryOrdering(primaryCharSystem: CharSystem): Ordering[OutputEntry] = new Ordering[OutputEntry] {
    override def compare(x: OutputEntry, y: OutputEntry): Int = {
      val xGraphemes: List[String] = Grapheme.splitIntoGraphemes(x.chineseStr)
      val yGraphemes: List[String] = Grapheme.splitIntoGraphemes(y.chineseStr)
//  穄   称
      if ((x.chineseStr == "穄" && y.chineseStr == "称" ) || (y.chineseStr == "穄" && x.chineseStr == "称" )) {
        val test = ""
      }
      val compareElementsResults: Int = compareElements(x, y, OutputSorting.elements)
      val compareSmallSingleResult: Int = compareSmallSingle(x, y, xGraphemes, yGraphemes, primaryCharSystem)
      val compareCedictResult: Int = compareCedict(x, y, simplifiedCedict, traditionalCedict, primaryCharSystem)
      val compareStrSize: Int = compareSize(xGraphemes, yGraphemes)
      val compareAnySingleResult: Int = compareJundaAndTzaiSingle(x, y, xGraphemes, yGraphemes, primaryCharSystem)
      val compareSinicaResult: Int = compareSinica(x.sinicaOptionSD, y.sinicaOptionSD, xGraphemes, yGraphemes, x, y)
      val compareBCLUResult: Int = compareBCLU(x.BCLUoptionBD, y.BCLUoptionBD, xGraphemes, yGraphemes, x, y)

      if (xGraphemes.length > 1 && yGraphemes.length > 1 && compareSinicaResult == 0 && compareBCLUResult == 0) {
        val test = ""
      }

      //order:
      // >1 comes first, then 1
      if (compareStrSize != 0) { return compareStrSize }
      // single characters under 5000 of the chrSystem
      if (compareSmallSingleResult != 0) {return compareSmallSingleResult}
      //cedict before non-cedict
      if (compareCedictResult != 0) {return compareCedictResult}
      // elements before non - elements
      if (compareElementsResults != 0) {return compareElementsResults}
      //charSystem before non-charSystem
      if (compareAnySingleResult != 0) {return compareAnySingleResult}
      // words frequency sorting
      val listOrderingJunda = compareLists(x.jundaReverseOrder, y.jundaReverseOrder)
      val listOrderingTzai = compareLists(x.tzaiReverseOrder, y.tzaiReverseOrder)
      if (primaryCharSystem == CharSystem.Junda) {
        if (compareBCLUResult != 0) {return compareBCLUResult}
        if (listOrderingJunda != 0) {return listOrderingJunda}
        if (compareSinicaResult != 0) {return compareSinicaResult}
        if (listOrderingTzai != 0) {return listOrderingTzai}
      } else if (primaryCharSystem == CharSystem.Tzai) {
        if (compareSinicaResult != 0) {return compareSinicaResult}
        if (listOrderingTzai != 0) {return listOrderingTzai}
        if (compareBCLUResult != 0) {return compareBCLUResult}
        if (listOrderingJunda != 0) {return listOrderingJunda}
      }

      if (x.chineseStr < y.chineseStr) { return 1 }
      if (y.chineseStr < x.chineseStr) { return -1 }
      throw new Exception("no known sorting tiebreak")
    }
  }

  private def compareSizeTwoBeforeOne(x: List[String], y: List[String]): Int = {
    // length 2 comes before length 1
    if (x.length == 2 && y.length == 1) {
      return -1
    } else if (y.length == 2 && x.length == 1) {
      return 1
    } else {
      return 0
    }
  }

  private def compareSize(x: List[String], y: List[String]): Int = {
    // length > 1 comes before length 1.
    if (x.length > 1 && y.length == 1) {
      return -1
    } else if (y.length > 1 && x.length == 1) {
      return 1
    } else {
      return 0
    }

  }

  private def compareElements(x: OutputEntry, y: OutputEntry, elems: Set[String]): Int = {
    try {
      val xIsInElem = elems.contains(x.chineseStr)
      val yIsInElem = elems.contains(y.chineseStr)

      if (xIsInElem && yIsInElem) {
        0
      } else if (xIsInElem) {
        -1
      } else if (yIsInElem) {
        1
      } else {
        0
      }
    } catch {
      case e: Exception => throw new Exception(e)
    }
  }

  private def compareCedict(x: OutputEntry, y: OutputEntry, 
                            simplifiedCedict: Set[String], traditionalCedict: Set[String], 
                            primaryCharSystem: CharSystem): Int = {
    val xInSimp: Boolean = simplifiedCedict.contains(x.chineseStr)
    val yInSimp: Boolean = simplifiedCedict.contains(x.chineseStr)
    val xInTrad: Boolean = traditionalCedict.contains(x.chineseStr)
    val yInTrad: Boolean = traditionalCedict.contains(x.chineseStr)
    
    if (primaryCharSystem == CharSystem.Junda) {
      if (xInSimp && !yInSimp) {
        -1
      } else if (!xInSimp && yInSimp) {
        1
      } else {
        0
      }
    } else if (primaryCharSystem == CharSystem.Tzai) {
      if (xInTrad && !yInTrad) {
        -1
      } else if (!xInTrad && yInTrad) {
        1
      } else {
        0
      }
    }
    0
  }

  private def compareSmallSingle(x: OutputEntry, y: OutputEntry,
                                 XinpGraphemes: List[String], YinpGraphemes: List[String],
                                 primaryCharSystem: CharSystem): Int = {
    if ((x.chineseStr == "穄" && y.chineseStr == "称") || (y.chineseStr == "穄" && x.chineseStr == "称")) {
      val test = ""
    }
    val singleXJunda: Int = singleUnder5000Junda(x)
    val singleYJunda: Int = singleUnder5000Junda(y)
    val singleXTzai: Int = singleUnder5000Tzai(x)
    val singleYTzai: Int = singleUnder5000Tzai(y)

    if (primaryCharSystem == CharSystem.Junda && (XinpGraphemes.length == 1) && (YinpGraphemes.length == 1)) {
      if (singleXJunda < singleYJunda) { return -1
      } else if (singleYJunda < singleXJunda) { return 1
      //} else if (singleXTzai < singleYTzai) { return -1
      //} else if (singleYTzai < singleXTzai) { return 1
      } else {return 0}
    } else if (primaryCharSystem == CharSystem.Tzai && (XinpGraphemes.length == 1) && (YinpGraphemes.length == 1)) {
      if (singleXTzai < singleYTzai) { return -1
      } else if (singleYTzai < singleXTzai) { return 1
      //} else if (singleXJunda < singleYJunda) { return -1
      //} else if (singleYJunda < singleXJunda) { return 1
      } else { return 0 }
    }
    // make sure a single character below 5000 comes before a 2+ character word
    if (primaryCharSystem == CharSystem.Junda
        && (XinpGraphemes.length == 1) && !(YinpGraphemes.length == 1) && singleXJunda <= 5000) {
      return -1
    }
    if (primaryCharSystem == CharSystem.Junda
      && !(XinpGraphemes.length == 1) && (YinpGraphemes.length == 1) && singleYJunda <= 5000) {
      return 1
    }
    if (primaryCharSystem == CharSystem.Tzai
      && (XinpGraphemes.length == 1) && !(YinpGraphemes.length == 1) && singleXTzai <= 5000) {
      return -1
    }
    if (primaryCharSystem == CharSystem.Tzai
      && !(XinpGraphemes.length == 1) && (YinpGraphemes.length == 1) && singleYTzai <= 5000) {
      return 1
    }
    return 0
  }

  private def compareJundaAndTzaiSingle(x: OutputEntry, y: OutputEntry,
                                        XinpGraphemes: List[String], YinpGraphemes: List[String],
                                        primaryCharSystem: CharSystem): Int = {
    if ((x.chineseStr == "穄" && y.chineseStr == "称") || (y.chineseStr == "穄" && x.chineseStr == "称")) {
      val test = ""
    }
    val singleXJunda: Int = singleJundaAllSize(x)
    val singleYJunda: Int = singleJundaAllSize(y)
    val singleXTzai: Int = singleTzaiAllSize(x)
    val singleYTzai: Int = singleTzaiAllSize(y)

    if (primaryCharSystem == CharSystem.Junda && (XinpGraphemes.length == 1) && (YinpGraphemes.length == 1)) {
      if (singleXJunda < singleYJunda) { return -1
      } else if (singleYJunda < singleXJunda) { return 1
      } else if (singleXTzai < singleYTzai) { return -1
      } else if (singleYTzai < singleXTzai) { return 1
      } else {return 0}
    } else if (primaryCharSystem == CharSystem.Tzai && (XinpGraphemes.length == 1) && (YinpGraphemes.length == 1)) {
      if (singleXTzai < singleYTzai) { return -1
      } else if (singleYTzai < singleXTzai) { return 1
      } else if (singleXJunda < singleYJunda) { return -1
      } else if (singleYJunda < singleXJunda) { return 1
      } else { return 0 }
    }
    return 0
  }

  private def singleJundaAllSize(inp: OutputEntry): Int = {
    val jundaNum: Int = if (inp.jundaReverseOrderG(0).junda.isDefined) {
      inp.jundaReverseOrderG(0).junda.get.ordinal
    } else {
      Int.MaxValue
    }
    return jundaNum
  }

  private def singleTzaiAllSize(inp: OutputEntry): Int = {
    val tzaiNum: Int = if (inp.tzaiReverseOrderG(0).tzai.isDefined) {
      inp.tzaiReverseOrderG(0).tzai.get.ordinal
    } else {
      Int.MaxValue
    }
    return tzaiNum
  }

  private def singleUnder5000Junda(inp: OutputEntry): Int = {
    val jundaNum: Int = if (inp.jundaReverseOrderG(0).junda.isDefined
      && inp.jundaReverseOrder(0) <= 5000) {
      inp.jundaReverseOrder(0)
    } else {
      Int.MaxValue
    }
    return jundaNum
  }

  private def singleUnder5000Tzai(inp: OutputEntry): Int = {
    val tzaiNum: Int = if (inp.tzaiReverseOrderG(0).tzai.isDefined
      && inp.tzaiReverseOrder(0) <= 5000) {
      inp.tzaiReverseOrder(0)
    } else {
      Int.MaxValue
    }
    return tzaiNum
  }

  private def compareBCLU(x: Option[BLCUData], y: Option[BLCUData],
                          xGraphemes: List[String], yGraphemes: List[String],
                          xFull: OutputEntry, yFull: OutputEntry): Int = {
    if ((x.isDefined) && !(y.isDefined)) return -1
    if (!(x.isDefined) && (y.isDefined)) return 1
    if (x.isDefined && y.isDefined) {
      if (x.get.ordinal < y.get.ordinal) { return -1 }
      if (y.get.ordinal < x.get.ordinal) { return 1 }
    }
    return 0
  }

  private def compareSinica(x: Option[SinicaData], y: Option[SinicaData],
                            xGraphemes: List[String], yGraphemes: List[String],
                            xFull: OutputEntry, yFull: OutputEntry): Int = {
    if ((x.isDefined) && !(y.isDefined))
      return -1
    if (!(x.isDefined) && (y.isDefined))
      return 1
    if (x.isDefined && y.isDefined) {
      if (x.get.ordinal < y.get.ordinal) {
        return -1
      }
      if (y.get.ordinal < x.get.ordinal) {
        return 1
      }
    }
    return 0
  }

  private def compareLists(list1: List[Int], list2: List[Int]): Int = {
    @annotation.tailrec
    def compareRecursive(l1: List[Int], l2: List[Int]): Int = {
      (l1, l2) match {
        case (Nil, Nil) => 0                      // Both lists are empty
        case (x :: _, Nil) => -1                   // Second list is empty
        case (Nil, y :: _) => 1                  // First list is empty
        case (x :: xs, y :: ys) =>
          if x < y then -1                        // First list is smaller
          else if x > y then 1                  // Second list is smaller
          else compareRecursive(xs, ys)          // Elements are equal, continue to next elements
      }
    }
    compareRecursive(list1, list2)
  }

}