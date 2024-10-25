package UtilityClasses

class ConwayColl(inputrawConway: Conway, inputunicode: String, inputchar: Grapheme) {
  
  def this(input: List[String], unicodeInput: String, charInput: Grapheme) = {
    this(ConwayColl.generateConwayColl(input), unicodeInput, charInput)
  }
  
  def this(character: Grapheme, rawConway: String) = {
    this(Conway(rawConway), ConwayColl.getUnicodeString(character.char), character)
  }

  val rawConway: Conway = inputrawConway
  val unicode: String = inputunicode
  val char: Grapheme = inputchar

}


object ConwayColl {

  private def generateConwayColl(genInput: List[String]): Conway = {
    if (genInput.size != 1) {
      throw new Exception("Conway Lines list greater than 1.")
    }
    Conway(genInput.head)
  }

  private def getUnicodeString(char: String): String = {
    if (char.length > 2) {
      throw new IllegalArgumentException("Input string should be a single character.")
    }

    val codePoint = if (char.length == 1) {
      char.codePointAt(0)
    } else {
      Character.toCodePoint(char.charAt(0), char.charAt(1))
    }
    f"U+${codePoint}%04X"
  }
}
