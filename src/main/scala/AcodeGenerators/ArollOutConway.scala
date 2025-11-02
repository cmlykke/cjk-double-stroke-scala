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

  def getRemainderFromMainAndElem(main: List[String],
                                  elem: List[String]): Set[String] = {
    val allZipped = main.flatMap(x => elem.map(y => (x, y)))
    return allZipped.map(x => mainCadidateMatch(x._1, x._2)).flatten.toSet
  }

  private def mainCadidateMatch(main: String,
                                     elem: String): Set[String] = {
    return mainCandidated(main, elem, main, elem)
  }

  private def mainCandidated(main: String,
                                               elem: String,
                                               mainOrig: String,
                                               elemOrig: String): Set[String] = {
    if (elem.length == 0) {
      return Set(main)
    }
    if (main.length == 0) {
      return Set()
    }
    if (main.head != '(' && (main.head == elem.head)) {
      return mainCandidated(main.tail, elem.tail, mainOrig, elemOrig)
    }
    if (main.head != '(' && elem.head != '(' && (main.head != elem.head)) {
      return Set()
    }
    if (main.head == '(') {
      val eachMainCandicate: List[String] = rolloutFirstParen(main)
      return eachMainCandicate.map(x => mainCandidated(x, elem, mainOrig, elemOrig)).flatten.toSet
    }
    if (elem.head == '(') {
      val eachElemCandicate: List[String] = rolloutFirstParen(elem)
      return eachElemCandicate.map(x => mainCandidated(main, x, mainOrig, elemOrig)).flatten.toSet
    }
    return Set()
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
