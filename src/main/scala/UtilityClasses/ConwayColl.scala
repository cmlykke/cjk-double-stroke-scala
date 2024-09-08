package UtilityClasses

class ConwayColl(input: List[String], unicodeInput: String, charInput: Grapheme) {
  val rawConway: Conway = generateConwayColl(input)
  val unicode: String = unicodeInput
  val char: Grapheme = charInput

  private def generateConwayColl(genInput: List[String]): Conway = {
    if (genInput.size != 1) {
      throw new Exception("Conway Lines list greate than 1.")
    }
    val res: Conway = Conway(genInput.head)
    return res
  }
}
