package AcodeGenerators

import Atypes.Agrapheme

object ArollOutConway {


  def returnMostLikelyMatch(conwayInput: List[String],
                                    elemInput: List[String],
                                    graph: Agrapheme,
                                    elemOpt: Option[String]): (String, String) = {
    if (elemInput.head == "1234") {
      val test = ""
    }

    var longestMatch = 0
    var currentmatchpair: (String, String) = ("", "")
    for (eachMain <- conwayInput) {
      for (eachElem <- elemInput) {
        val commonPrefixLength: Int = eachMain.zip(eachElem).takeWhile(_ == _).length
        if (commonPrefixLength > longestMatch) {
          longestMatch = commonPrefixLength
          currentmatchpair = (eachMain, eachElem)
        }
      }
    }
    if (longestMatch == 0) {
      return (conwayInput.head, elemInput.head)
    }
    if (currentmatchpair == ("","")) {
      val test2 = ""
    }
    if (currentmatchpair._1.isEmpty || currentmatchpair._2.isEmpty) {
      val test = ""
    }
    return currentmatchpair
  }
  
  def roolOutConway(input: String):  Set[String] = {
    val result =  roolOutConwayHelper(List(input))
    return result
  }

  private def roolOutConwayHelper(input: List[String]): Set[String] = {
    var results: Set[String] = Set()
    for (eachinput <- input) {
      if (eachinput.contains('(')) {
        val rolledOutFirstParen: List[String] = rolloutFirstParen(eachinput)
        val beforeFirst: String = beforeFirstparen(eachinput)
        val firstRemoved: List[String] =  rolledOutFirstParen.map(x =>beforeFirst + x)
        results = results ++ roolOutConwayHelper(firstRemoved)
      }
      else {
        results = results ++ List(eachinput)
      }
    }
    return results
  }

  def rolloutFirstParen(input: String): List[String] = {
    val parenContent: Option[String] = extractBetweenFirstParens(input)
    val parencontentSplit: List[String] = parenContent.get.split("\\|").toList
    val afterParen: Option[String] = removeFirstParen(input)
    val allCombi: List[String] = parencontentSplit.map(x => x + afterParen.get)
    return allCombi
  }

  def beforeFirstparen(input: String): String = {
    val indexOfFirstParen = input.indexOf('(')
    return input.substring(0, indexOfFirstParen)
  }

  def extractBetweenFirstParens(str: String): Option[String] = {
    val start = str.indexOf('(')
    if (start == -1) None
    else {
      val end = str.indexOf(')', start + 1)
      if (end == -1) None
      else Some(str.substring(start + 1, end))
    }
  }

  def removeFirstParen(input: String): Option[String] = {
    val start = input.indexOf(')')
    if (start == -1) {
      None
    } else {
      val result = Some(input.substring(start + 1))
      return result
    }
  }
}
