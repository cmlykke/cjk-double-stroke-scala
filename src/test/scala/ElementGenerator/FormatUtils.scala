package ElementGenerator

import UtilityClasses.{StaticFileCharInfoWithLetterConway, CharSystem}

object FormatUtils {


  def summarizeOverlap(
                        overlapData: List[(Int, List[StaticFileCharInfoWithLetterConway])]
                      ): String = {

    // Extract the first three keys (integers)
    val firstThreeKeys = overlapData.take(3).map(_._1).mkString(", ")

    // Calculate the total number of keys
    val totalKeys = overlapData.length

    // Calculate the total number of StaticFileCharInfoWithLetterConway objects beyond index 8
    val totalStaticFileCharInfoObjects = overlapData.flatMap { case (_, charList) =>
      charList.drop(9)
    }.size

    // Format the result string
    s"First 3 keys: [$firstThreeKeys], Total keys: $totalKeys, Total beyond index 8: $totalStaticFileCharInfoObjects"
  }
  
  def formatFinalRes(
                      finalres: List[(Int, List[StaticFileCharInfoWithLetterConway])],
                      charSystem: CharSystem
                    ): String = {
    finalres.zipWithIndex.map { case ((num, charInfoList), idx) =>
      // Start the line with the index formatted as a two-digit number
      val lineStart = f"$idx%02d" + " -"

      // Look in the first StaticFileCharInfoWithLetterConway element of the list
      val additionalPrefix = charInfoList.headOption.flatMap { headCharInfo =>
        headCharInfo.letterConway.find(_.conwayPairs.length == 4).map(_.conwayPairs.mkString(" "))
      }.getOrElse("")

      // Process each StaticFileCharInfoWithLetterConway with index greater than 8
      val details = charInfoList.zipWithIndex.collect {
        case (charInfo, index) if index > 8 =>
          val char = charInfo.grapheme.char
          val ordinal = charSystem match {
            case CharSystem.Junda => charInfo.grapheme.junda.map(_.ordinal).getOrElse("")
            case CharSystem.Tzai => charInfo.grapheme.tzai.map(_.ordinal).getOrElse("")
          }
          s"$char$ordinal"
      }.mkString(" ")

      // Combine the line start, additional prefix, and details, and structure with hyphens
      s"$lineStart $additionalPrefix - $details".trim
    }.mkString("\n") // Join all lines with a newline character
  }

}
