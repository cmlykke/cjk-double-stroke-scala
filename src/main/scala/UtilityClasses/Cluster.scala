package UtilityClasses

import com.ibm.icu.text.BreakIterator
import com.ibm.icu.util.ULocale
//import java.text.BreakIterator
import java.util

class Cluster(input: String) {
  val graphemes: List[Grapheme] = generateGraphemes(input)
  
  def noIdsShapeCharacters(): List[Grapheme] = {
    graphemes.filter(g => !startsWithIdeographicDescription(g.char))
  }
  
  private def generateGraphemes(input: String): List[Grapheme] = {
    val graphemeList: List[String] = Grapheme.splitIntoGraphemes(input)
    graphemeList.map(x => new Grapheme(x)).toList
  }
  
  private def startsWithIdeographicDescription(s: String): Boolean = {
    val codePoint = s.codePointAt(0)
    codePoint >= 0x2FF0 && codePoint <= 0x2FFF
  }

  override def equals(obj: Any): Boolean = obj match {
    case c: Cluster =>
      c.graphemes.length == this.graphemes.length &&
        (c.graphemes zip this.graphemes).forall { case (a, b) => a == b }
    case _ => false
  }

  override def hashCode(): Int =
    graphemes.map(_.hashCode).foldLeft(0)((a, b) => 31 * a + b)

}
