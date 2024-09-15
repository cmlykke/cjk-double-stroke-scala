package OutputTranslation

import UtilityClasses.ConwayUnambigous

import scala.collection.mutable

object TranslationFunctions {

  def translateVersionOne(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val mutableSet = mutable.Set[String]()
    unambigous.length match {
      case 1 => generateReadyCodeForOne(unambigous)
      case 2 => generateReadyCodeForTwo(unambigous)
      case 3 => generateReadyCodeForThree(unambigous)
      case 4 => generateReadyCodeForFour(unambigous)
      case n if n > 4 => generateReadyCodeOverFive(unambigous)
      case _ => throw IllegalArgumentException("translateVersionOne" + " does not have any codes")
    }
  }

  private def generateReadyCodeForOne(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val resultSet: mutable.Set[String] = mutable.Set[String]()
    val codes = unambigous.flatten
    for (code <- codes) {
      resultSet.add(code.conwayPairs.mkString(""))
    }
    resultSet.toSet
  }

  private def generateReadyCodeForTwo(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
    val combinations: List[List[ConwayUnambigous]] = generateCombinations(unambigous)
    var res: mutable.Set[String] = mutable.Set[String]()
    for (conLi <- combinations) {
      val part1: String = getFirstAndLast(conLi(0))
      val part2: String = getFirstSecondAndLast(conLi(1))
      res.add(part1+part2)
    }
    return res.toSet
  }

  private def generateReadyCodeForThree(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
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


  private def generateReadyCodeOverFive(unambigous: List[Set[ConwayUnambigous]]): Set[String] = {
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

}
