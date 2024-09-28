package OutputTranslation.TranslationHelpers

import UtilityClasses.ConwayUnambigous

import scala.collection.mutable

object TranslationFunctions3 {

  def generateReadyCodeForOne(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val resultSet: mutable.Set[String] = mutable.Set[String]()
    val codes = unambigous.flatten
    for (code <- codes) {
      resultSet.add(code.conwayPairs.mkString(""))
    }
    resultSet.toSet
  }

  def generateReadyCodeForTwo(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirstAndLast(conLi(0))
      val part2: String = getFirstSecondAndLast(conLi(1))
      res.add(part1+part2)
    }
    val finalRes = setToLength(res.toSet, 5)
    return finalRes
  }

  def generateReadyCodeForThree(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirstAndLast(conLi(0))
      val part2: String = getFirstAndLast(conLi(1))
      val part3: String = getFirstAndLast(conLi(2))
      res.add(part1 + part2 + part3)
    }
    val finalRes = padStrings(res.toSet, 6)
    return finalRes
  }


  def generateReadyCodeForFour(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirst(conLi(0))
      val part2: String = getFirst(conLi(1))
      val part3: String = getFirstAndLast(conLi(2))
      val part4: String = getFirstAndLast(conLi(3))
      res.add(part1 + part2 + part3 + part4)
    }
    val finalRes = padStrings(res.toSet, 6)
    return finalRes
  }


  def generateReadyCodeForFive(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirst(conLi(0))
      val part2: String = getFirst(conLi(1))
      val part3: String = getFirst(conLi(2))
      val part4: String = getFirst(conLi(3))
      val part5: String = getFirstAndLast(conLi(4))
      res.add(part1 + part2 + part3 + part4 + part5)
    }
    val finalRes = padStrings(res.toSet, 6)
    return finalRes
  }

  def generateReadyCodeOverSix(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirst(conLi(0))
      val part2: String = getFirst(conLi(1))
      val part3: String = getFirst(conLi(2))
      val part4: String = getFirst(conLi(3))
      val part5: String = getFirst(conLi(4))
      val part6: String = getFirst(conLi(5))
      res.add(part1 + part2 + part3 + part4 + part5 + part6)
    }
    val finalRes = padStrings(res.toSet, 6)
    return finalRes
  }

  private def getFirst(con: ConwayUnambigous): String = {
    con.conwayPairs.length match {
      case n if n > 0 => con.conwayPairs.head
      case _ => throw new IllegalArgumentException("the conway is empty")
    }
  }

  private def getFirstSecondAndLast(con: ConwayUnambigous): String = {
    val res: String = con.conwayPairs.length match {
      case n if n > 0 && n < 4 => con.conwayPairs.mkString("")
      case 4 => con.conwayPairs(0) + con.conwayPairs(1) + con.conwayPairs(3)
      case n if n > 4 => con.conwayPairs.head + con.conwayPairs(1) + con.conwayPairs.last
    }
    res
    //return con.conwayPairs(0)
  }

  private def getFirstAndLast(con: ConwayUnambigous): String = {
    val res: String = con.conwayPairs.length match {
      case 1 => con.conwayPairs(0)
      case n if n > 1 => con.conwayPairs.head + con.conwayPairs.last
    }
    res
    //return con.conwayPairs(0)
  }

  private def generateCombinations(sets: List[Set[ConwayUnambigous]]): List[List[ConwayUnambigous]] = {
    sets match {
      case Nil => List(Nil)
      case head :: tail =>
        for {
          element <- head.toList
          combination <- generateCombinations(tail)
        } yield element :: combination
    }
  }

  def padStrings(strings: Set[String], length: Int): Set[String] = {
    strings.map { str =>
      if (str.length > length) {
        throw new IllegalArgumentException(s"String '$str' has more characters than $length")
      } else {
        var paddedStr = str
        while (paddedStr.length < length) {
          paddedStr += "z"
        }
        paddedStr
      }
    }
  }

  def setToLength(strings: Set[String], length: Int): Set[String] = {
    strings.map { str =>
      if (str.length > length) {
        str.take(length)
      } else {
        var paddedStr = str
        while (paddedStr.length < length) {
          paddedStr += "z"
        }
        paddedStr
      }
    }
  }

}
