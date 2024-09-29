package ElementGenerator

import UtilityClasses.{ConwayUnambigous, Grapheme, StaticFileCharInfoWithLetterConway}

import scala.collection.mutable

class ElementTranslateToAlphabet {


  def createMapFromSet(inputSet: Set[StaticFileCharInfoWithLetterConway]):
  Map[Grapheme, StaticFileCharInfoWithLetterConway] = {
    inputSet.map(item => item.grapheme -> item).toMap
  }

  def generate8000Junda(): Set[StaticFileCharInfoWithLetterConway] = {
    val ajustClass = ElementAdjustedCodes
    val overlap8000 = ElementAdjustedCodes.elemAdjusted8000Junda
    val res = translateToAlphabetByMap(
      overlap8000,
      ElementTranslateToAlphabet.thirdChoiceMap)
    return res
  }


  def generateTranslatedAllChars(): Set[StaticFileCharInfoWithLetterConway] = {
    //val ajustClass = ElementAdjustedCodes
    val allChars = ElementAdjustedCodes.elemAdjustedAllChars
    val res = translateToAlphabetByMap(
      allChars,
      ElementTranslateToAlphabet.thirdChoiceMap)
    return res
  }

  def generate8000Tzai(): Set[StaticFileCharInfoWithLetterConway] = {
    val ajustClass = ElementAdjustedCodes
    val overlap8000 = ElementAdjustedCodes.elemAdjusted8000Tzai
    val res = translateToAlphabetByMap(
      overlap8000,
      ElementTranslateToAlphabet.thirdChoiceMap)
    return res
  }


  def translateToAlphabetByMap(charsWithProtoElems: Set[StaticFileCharInfoWithLetterConway],
                               translateToFinalKeyboard: Map[String, String])
  : Set[StaticFileCharInfoWithLetterConway] = {
    val tempres: mutable.Set[StaticFileCharInfoWithLetterConway] = mutable.Set()
    charsWithProtoElems.foreach(elems => {
      val codesToBeTranslated: Set[ConwayUnambigous] = elems.letterConway

      val translatedCodes = codesToBeTranslated.map { conway =>
        val translatedList = conway.conwayPairs.map { code =>
          translateToFinalKeyboard.getOrElse(code, throw new IllegalArgumentException(s"Key $code not found in translateToFinalKeyboard map"))
        }
        new ConwayUnambigous(translatedList, conway.is4Code)
      }

      // Create a new instance of StaticFileCharInfoWithLetterConway
      val newElems = new StaticFileCharInfoWithLetterConway(elems.decorated, translatedCodes)
      tempres += newElems
    })

    tempres.toSet
  }
}

object ElementTranslateToAlphabet {
  private val translatorInstance = new ElementTranslateToAlphabet()

  val currentChoice8000Juda: Set[StaticFileCharInfoWithLetterConway] = translatorInstance.generate8000Junda()
  val currentChoice8000Tzai: Set[StaticFileCharInfoWithLetterConway] = translatorInstance.generate8000Tzai()
  val basicConwayMap: Map[Grapheme, StaticFileCharInfoWithLetterConway] =
    translatorInstance.createMapFromSet(translatorInstance.generateTranslatedAllChars())
    

  val tras = new ElementTranslateToAlphabet() //ElementTranslateToAlphabet
  val allch: Set[StaticFileCharInfoWithLetterConway] = tras.generateTranslatedAllChars() //generateTranslatedAllChars   ElementTranslateToAlphabet
  val completeTranslatedConwayMap: Map[Grapheme, StaticFileCharInfoWithLetterConway] = tras.createMapFromSet(allch)
  val kuntest: String = ""


  // Static function returning a hardcoded map of string to string
  //top line is 4-3, 
  //middleline is 5-1, 
  //bottomline is 21-25

  def thirdChoiceMap: Map[String, String] = {
    Map(
      "a" -> "d",//"k",//53
      "b" -> "j",//"f",//12
      "c" -> "f",//"j",//52
      "d" -> "s",//"l",//54
      "e" -> "l",//"s",//14
      "f" -> "k",//"d",//13
      //"g" -> "Cinco",
      //"h" -> "Cinco",
      //"i" -> "Cinco",
      //"j" -> "Cinco",
      "k" -> "i",//i
      "l" -> "u",//u
      "m" -> "r",//r
      "n" -> "p",//p
      "o" -> "w",//w
      "p" -> "o",//o
      "q" -> "e",//
      //"r" -> "Cinco",
      //"s" -> "Cinco",

      "1" -> "h",//"g",
      "11" -> "h",//"g",
      "12" -> "j",//"f",
      "13" -> "k",//"d",
      "14" -> "l",//"s",
      "15" -> "m",//"a",

      "2" -> "x",
      "21" -> "x",
      "22" -> "c",
      "23" -> "v",
      "24" -> "b",
      "25" -> "n",

      "3" -> "y",
      "31" -> "y",
      "32" -> "u",
      "33" -> "i",
      "34" -> "o",
      "35" -> "p",

      "4" -> "t",
      "41" -> "t",
      "42" -> "r",
      "43" -> "e",
      "44" -> "w",
      "45" -> "q",

      "5" -> "g",//"h",
      "51" -> "g",//"h",
      "52" -> "f",//"j",
      "53" -> "d",//"k",
      "54" -> "s",//"l",
      "55" -> "a"//"m"
    )
  }
  
  // Static function returning a hardcoded map of string to string
  //top line is 4-3, 
  //middleline is 1-5, 
  //bottomline is 21-25
  
  //wer -> 驤 車 錶      uiop ->  縱 言 餞 𠁣𠃛 
  //sdf -> 扌 目 口止    jkl -> 𠂊亅⿱𠂊亅 木  虫  
  
  def secondChoiceMap: Map[String, String] = {
    Map(
      "a" -> "k",//53
      "b" -> "f",//12
      "c" -> "j",//52
      "d" -> "l",//54
      "e" -> "s",//14
      "f" -> "d",//13
      //"g" -> "Cinco",
      //"h" -> "Cinco",
      //"i" -> "Cinco",
      //"j" -> "Cinco",
      "k" -> "i",//i
      "l" -> "u",//u
      "m" -> "r",//r
      "n" -> "p",//p
      "o" -> "w",//w
      "p" -> "o",//o
      "q" -> "e",//
      //"r" -> "Cinco",
      //"s" -> "Cinco",

      "1" -> "g",
      "11" -> "g",
      "12" -> "f",
      "13" -> "d",
      "14" -> "s",
      "15" -> "a",

      "2" -> "x",
      "21" -> "x",
      "22" -> "c",
      "23" -> "v",
      "24" -> "b",
      "25" -> "n",

      "3" -> "y",
      "31" -> "y",
      "32" -> "u",
      "33" -> "i",
      "34" -> "o",
      "35" -> "p",

      "4" -> "t",
      "41" -> "t",
      "42" -> "r",
      "43" -> "e",
      "44" -> "w",
      "45" -> "q",

      "5" -> "h",
      "51" -> "h",
      "52" -> "j",
      "53" -> "k",
      "54" -> "l",
      "55" -> "m"
    )
  }

  // Static function returning a hardcoded map of string to string
  //top line is 3-4, elems are wer-uiop
  //middleline is 5-1, elems are sdf-jkl
  //bottomline is 25-21
  def firstChoiceMap: Map[String, String] = {
    Map(
      "a" -> "d",//53
      "b" -> "k",//13
      "c" -> "s",//54
      "d" -> "j",//12
      "e" -> "l",//14
      "f" -> "f",//52
      //"g" -> "Cinco",
      //"h" -> "Cinco",
      //"i" -> "Cinco",
      //"j" -> "Cinco",
      "k" -> "e",//33
      "l" -> "i",//43
      "m" -> "u",//42
      "n" -> "o",//44
      "o" -> "r",//32
      "p" -> "p",//45
      "q" -> "w",//34
      //"r" -> "Cinco",
      //"s" -> "Cinco",

      "1" -> "h",
      "11" -> "h",
      "12" -> "j",
      "13" -> "k",
      "14" -> "l",
      "15" -> "m",

      "2" -> "n",
      "21" -> "n",
      "22" -> "b",
      "23" -> "v",
      "24" -> "c",
      "25" -> "x",

      "3" -> "t",
      "31" -> "t",
      "32" -> "r",
      "33" -> "e",
      "34" -> "w",
      "35" -> "q",

      "4" -> "y",
      "41" -> "y",
      "42" -> "u",
      "43" -> "i",
      "44" -> "o",
      "45" -> "p",

      "5" -> "g",
      "51" -> "g",
      "52" -> "f",
      "53" -> "d",
      "54" -> "s",
      "55" -> "a"
    )
  }
}
