package UtilityClasses

import staticFileGenerators.Conway.GenerateConwayCodes

class CedictSubEntry(chineseStr: String,
                     inputGrap: Grapheme,
                     //maybeInfo: Option[StaticFileCharInfoWithLetterConway],
                     charMap: Map[Grapheme, StaticFileCharInfoWithLetterConway]) {

  if (inputGrap.char.equals("𪢌")) { //U+51A0	冠  20896
    val test = ""
  }

  val maybeInfoVar: Option[StaticFileCharInfoWithLetterConway] = charMap.get(inputGrap)
  val isAlphabet: Option[Char] = checkIsAlphabet(inputGrap)


  def checkIsAlphabet(inputGrap: Grapheme): Option[Char] = {
    var res: Option[Char] = None
    val mapper = new GenerateConwayCodes()
    val allChars: Set[Grapheme] = mapper.getConwayCharacters
    val char = inputGrap.char.head // Extracting the single character

    // ⺮ (U+2EAE)
    if (isEnglishAlphabet(char)) {
      // Check if the character is alphabetic and return the lowercase version
      res = Some(char.toLower)
    } else if ('、'.equals(char)) { //12289 “、” (U+3001)   // ⺮ ⺮” (U+2EAE)  11950
      val tes = ""
    } else if ( !(char >= 12288 && char <= 12351) &&  //U+3000..U+303F  12288--12351
      ((char < 11904 || char > 12031) &&// U+2E80..U+2EFF--11904 12031
      (char <= 12735 ||  //12735 U+31BF     
      (char >= 40960 && // U+A000
        char <= 55295) || // D7FF //65535)) // U+FFFF
      (char >= 63744 && // F900
        char <= 65535)))) /// U+FFFF
    { //11903 - U+2E7F Supplemental Punctuation
      // Check if the character is ASCII
      res = Some('z')
    }

    if (res.isDefined && !(maybeInfoVar == null) &&  maybeInfoVar.isDefined) {
      throw new IllegalArgumentException(s"Character '${char}' should not have staticInfo.")
    }
    if (res.isDefined && allChars.contains(inputGrap)) {
      throw new IllegalArgumentException(s"Character '${char}' should not be char and be in allChars.")
    }
    if (!(maybeInfoVar == null) && maybeInfoVar.isDefined) {
      res = None;
    } else if (!res.isDefined) {
      res = None
      // use this error code to create new conway codes
      //throw new IllegalArgumentException(s"Character '${char}' is not known CJK or ASCII.")
    }
    return res
  }

  def isEnglishAlphabet(c: Char): Boolean = {
    c match {
      case ch if ('a' to 'z').contains(ch) => true
      case ch if ('A' to 'Z').contains(ch) => true
      case _ => false
    }
  }
}
