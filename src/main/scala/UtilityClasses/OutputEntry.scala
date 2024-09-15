package UtilityClasses

class OutputEntry(inputChineseStr: String,
                  inputMeaning: String,
                  inputPronounciation: String,
                  inputTradSimp: String,
                  inpjundaReverseOrder: List[Grapheme],
                  inptzaiReverseOrder: List[Grapheme],
                  inpcodes: Set[String]) {
  val chineseStr: String = inputChineseStr
  val meaning: String = inputMeaning
  val pron: String = inputPronounciation
  val tradSimp: String = inputTradSimp
  val jundaReverseOrder: List[Grapheme] = inpjundaReverseOrder
  val tzaiReverseOrder: List[Grapheme] = inptzaiReverseOrder
  val codes: Set[String] = inpcodes

  override def equals(obj: Any): Boolean = obj match {
    case g: OutputEntry => g.chineseStr == this.chineseStr
    case _           => false
  }

  override def hashCode(): Int = chineseStr.hashCode
}
