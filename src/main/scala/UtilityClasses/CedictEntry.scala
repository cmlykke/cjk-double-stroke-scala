package UtilityClasses

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CedictEntry(chineseStrInp: String, systemInp: CharSystem,
                  charMap: Map[Grapheme, StaticFileCharInfoWithLetterConway],
                  inputMeaning: String,
                  inputPronounciation: String,
                  inputTradSimp: String) {
  val chineseStr: String = chineseStrInp
  val meaning: String = inputMeaning
  val pronounciation: String = inputPronounciation
  val tradSimp: String = inputTradSimp
  val chineseStrGraphemes: Set[Grapheme] = 
    Grapheme.splitIntoGraphemes(chineseStr).map(Grapheme(_)).toSet
  val cedict: List[CedictSubEntry] = generateSubEntry(chineseStr, charMap)
  val unambigous: List[Set[ConwayUnambigous]] = generateUnambique(cedict)
  val system = systemInp
  val readyCodes: Set[String] = if (unambigous.isEmpty) Set.empty else generateReadyCodes(chineseStrInp, unambigous, cedict)
  //val readyCodes: Set[String] = generateReadyCodes(chineseStrInp, unambigous, cedict)


  // Override equals
  override def equals(obj: Any): Boolean = obj match {
    case that: CedictEntry => this.chineseStr == that.chineseStr
    case _ => false
  }

  // Override hashCode
  override def hashCode(): Int = chineseStr.hashCode()


  protected def generateSubEntry(chars: String,
                                 charMap: Map[Grapheme, StaticFileCharInfoWithLetterConway]): List[CedictSubEntry] = {
    val graphemes: List[String] = Grapheme.splitIntoGraphemes(chars)
    graphemes.toList.map { grapStr =>
      val grap: Grapheme = Grapheme(grapStr)
      new CedictSubEntry(chars, grap, charMap)
    }
  }

  protected def generateUnambique(subentry: List[CedictSubEntry]): List[Set[ConwayUnambigous]] = {
    val res: ListBuffer[Set[ConwayUnambigous]] = ListBuffer()

    for (cedict <- subentry) {
      if (cedict.isAlphabet.isDefined) {
        val alphaConway: Set[ConwayUnambigous] = Set(new ConwayUnambigous(List(cedict.isAlphabet.get.toString), true))
        res.append(alphaConway)
      } else if (cedict.maybeInfoVar.isDefined) {
        val alphaCode: Set[ConwayUnambigous] = cedict.maybeInfoVar.get.letterConway //alphabet codes for the character
        val alphaCodeToList: Set[ConwayUnambigous] = alphaCode.filter(_.is4Code).toSet
        res.append(alphaCodeToList)
      } else {
        return List()
        //throw new IllegalArgumentException("subentry is not welformed")
      }
    }
    res.toList
  }

  protected def generateReadyCodes(chineseStr: String,
                                   unambigous: List[Set[ConwayUnambigous]],
                                   subEntry: List[CedictSubEntry]): Set[String] = {
    val unambigousCodes = null //unambigous.flatMap(_.codes).toSet
    val subEntryCodes = null //subEntry.flatMap(_.codes).toSet
    val mutableSet = mutable.Set[String]()
    unambigous.length match {
      case 1 => generateReadyCodeForOne(unambigous)
      case 2 => generateReadyCodeForTwo(unambigous)
      case 3 => generateReadyCodeForThree(unambigous)
      case 4 => generateReadyCodeForFour(unambigous)
      case n if n > 4 => generateReadyCodeOverFive(unambigous)
      case _ => throw IllegalArgumentException(chineseStr + " does not have any codes")
    }
    return Set()
  }

  protected def generateReadyCodeForOne(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val resultSet: mutable.Set[String] = mutable.Set[String]()
    val codes = unambigous.flatten
    for (code <- codes) {
      resultSet.add(code.conwayPairs.mkString(""))
    }
    resultSet.toSet
  }

  protected def generateReadyCodeForTwo(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirstAndLast(conLi(0))
      val part2: String = getFirstSecondAndLast(conLi(1))
      res.add(part1+part2)
    }
    return res.toSet
  }

  protected def generateReadyCodeForThree(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirst(conLi(0))
      val part2: String = getFirstAndLast(conLi(1))
      val part3: String = getFirstAndLast(conLi(2))
      res.add(part1 + part2 + part3)
    }
    return res.toSet
  }


  protected def generateReadyCodeForFour(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirst(conLi(0))
      val part2: String = getFirst(conLi(1))
      val part3: String = getFirst(conLi(2))
      val part4: String = getFirstAndLast(conLi(3))
      res.add(part1 + part2 + part3 + part4)
    }
    return res.toSet
  }


  protected def generateReadyCodeOverFive(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirst(conLi(0))
      val part2: String = getFirst(conLi(1))
      val part3: String = getFirst(conLi(2))
      val part4: String = getFirst(conLi(3))
      val part5: String = getFirst(conLi(4))
      res.add(part1 + part2 + part3 + part4 + part5)
    }
    return res.toSet
  }
  
  protected def getFirst(con: ConwayUnambigous): String = {
    con.conwayPairs.length match {
      case n if n > 0 => con.conwayPairs.head
      case _ => throw new IllegalArgumentException("the conway is empty")
    }
  }

  protected def getFirstSecondAndLast(con: ConwayUnambigous): String = {
    con.conwayPairs.length match {
      case n if n > 0 && n < 4 => con.conwayPairs.mkString("")
      case 4 => con.conwayPairs(0) + con.conwayPairs(1) + con.conwayPairs(3)
      case n if n > 4 => con.conwayPairs.head + con.conwayPairs(1) + con.conwayPairs.last
    }
    return con.conwayPairs(0)
  }
  
  protected def getFirstAndLast(con: ConwayUnambigous): String = {
    con.conwayPairs.length match {
      case 1 => con.conwayPairs(0)
      case n if n > 1 => con.conwayPairs.head + con.conwayPairs.last
    }
    return con.conwayPairs(0)
  }
  
  def generateCombinations(sets: List[Set[ConwayUnambigous]]): List[List[ConwayUnambigous]] = {
    sets match {
      case Nil => List(Nil)
      case head :: tail =>
        for {
          element <- head.toList
          combination <- generateCombinations(tail)
        } yield element :: combination
    }
  }

}
