package Atypes

case class AcedictEntry(rawEntry: String) {
  val entry: List[Agrapheme] = AcedictEntry.splitIntoGraphemeList(rawEntry)

  override def equals(obj: Any): Boolean = obj match {
    case that: AcedictEntry => this.entry == that.entry
    case _ => false
  }

  override def hashCode(): Int = entry.hashCode
}

object AcedictEntry {
  def splitIntoGraphemeList(input: String): List[Agrapheme] = {
    input.codePoints().toArray
      .map(codepoint => String(Character.toChars(codepoint)))
      .map(x => Agrapheme(x)).toList
  }
}
