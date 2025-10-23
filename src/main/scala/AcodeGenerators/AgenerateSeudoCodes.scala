package AcodeGenerators

import Adatasources.ManualData.AcodelengthRules
import Atypes.PossibleWordCodes.{FirstCode, FirstLastCode, FirstSecondLastCode}
import Atypes.{AsortingCriteria, PossibleWordCodes, SortingCodes}

object AgenerateSeudoCodes {

  val localFill: String = AcodelengthRules.fill

  def splitCodeListMultiChar(inp: List[String], multiCharType: PossibleWordCodes): List[String] = {
    val res = splitCodeListHelperMultiChar(List(), (inp, multiCharType))
    return res
  }

  def splitCodeListHelperMultiChar(reslist: List[String],
                                   inp: (List[String], PossibleWordCodes)): List[String] = {
    if (inp._2 == FirstCode) {
      return firstCode(List(), inp._1)
    }
    if (inp._2 == FirstLastCode) {
      return firstLastCode(List(), inp._1)
    }
    if (inp._2 == FirstSecondLastCode) {
      return firstSecondLastCode(List(), inp._1)
    }
    throw new RuntimeException("unhandled possibleWordCode")
  }

  private def generateNoFillFromFourCode(input: Set[(List[String], AsortingCriteria)]):
                                   Set[(List[String], AsortingCriteria)] = {
    input.filter(x => x._2 == SortingCodes.FourCode).map(x => removeZFillFromFourCode(x._1)).toSet
  }

  private def removeZFillFromFourCode(codelist: List[String]): (List[String], AsortingCriteria) = {
    if (codelist.size == 0) {
      throw new RuntimeException("removeZFill should not result in a zero length code list")
    }
    if (codelist.last != localFill) {
      if (codelist.length == 1) {
        return (codelist, SortingCodes.OneCode)
      }
      if (codelist.length == 2) {
        return (codelist, SortingCodes.TwoCode)
      }
      if (codelist.length == 3) {
        return (codelist, SortingCodes.ThreeCode)
      }
      if (codelist.length == 4) {
        return (codelist, SortingCodes.FourCode)
      }
      throw new RuntimeException("unexpected code length for removeing fill characters")
    }
    return removeZFillFromFourCode(codelist.init)
  }

  def splitCodeListSingleChar(inp: (List[String],Int),
                                      codeStructure: PossibleWordCodes): (List[String], AsortingCriteria) = {
    splitCodeListHelperSingleChar(List(), inp._1, codeStructure)
  }

  private def firstSecondLastCode(reslist: List[String],
                            inp: List[String]): List[String] = {
    val inpIsEmpty: Boolean = inp.length == 0 || inp.head.length == 0
    val inpHasInitialElement: Boolean = inp.size > 1 && AcodelengthRules.elementTypes.contains(inp.head)
    val inpHasOneLetterCodeLeft: Boolean = inp.size == 1 && AcodelengthRules.elementTypes.contains(inp.head)
    if (reslist.size == 3 || inpIsEmpty) {
      return reslist
    }
    if (inpHasInitialElement) {
      val firstElem = firstSecondLastCode(reslist.appended(inp.head), inp.tail)
      return List(inp.head) ++ firstLastCode(List(), inp.tail)
    }
    if (inpHasOneLetterCodeLeft) {
      val firstElem = firstSecondLastCode(reslist.appended(inp.head), List())
      return List(inp.head)
    }
    if ( !inpHasOneLetterCodeLeft) {
      val firstElem = firstSecondLastCode(reslist.appended(inp.head.take(2)), List(inp.head.drop(2)))
      return List(inp.head.take(2)) ++ firstLastCode(List(), List(inp.head.drop(2)))
    }
    throw RuntimeException("unhandled state in firstSecondLastCode")
  }

  private def firstLastCode(reslist: List[String],
                            inp: List[String]): List[String] = {
    val inpIsEmpty: Boolean = inp.length == 0 || inp.head.length == 0
    val inpHasInitialElement: Boolean = inp.size > 1 && AcodelengthRules.elementTypes.contains(inp.head)
    val inpHasOneLetterCodeLeft: Boolean = inp.size == 1 && AcodelengthRules.elementTypes.contains(inp.head)
    if (reslist.size == 2 || inpIsEmpty) {
      return reslist
    }
    if (reslist.size == 0 && inpHasInitialElement) {
      return firstLastCode(reslist.appended(inp.head), inp.tail)
    }
    if (reslist.size == 0 && inpHasOneLetterCodeLeft) {
      return firstLastCode(reslist.appended(inp.head), List())
    }
    if (reslist.size == 0 && !inpHasOneLetterCodeLeft) {
      return firstLastCode(reslist.appended(inp.head.take(2)), List(inp.head.drop(2)))
    }
    if (reslist.size == 1 && inp.last.size > 2) {
      return firstLastCode(reslist.appended(inp.last.takeRight(2)), List(inp.last.dropRight(2)))
    }
    if (reslist.size == 1 && inp.last.size < 3) {
      return firstLastCode(reslist.appended(inp.last), List())
    }
    throw RuntimeException("unhandled state in firstLastCode")
  }

  private def firstCode(reslist: List[String],
                        inp: List[String]): List[String] = {
    if (reslist.size == 1) {
      return reslist
    }
    if (inp.length > 1) {
      return List(inp.head)
    }
    if (inp.length == 1) {
      if (inp.head.length > 1) {
        return List(inp.head.substring(0,2))
      } else if (inp.head.length != 0){
        return inp
      }
    }
    throw RuntimeException("unhandled state in firstCode")
  }

  private def splitCodeListHelperSingleChar(reslist: List[String],
                                            inp: List[String],
                                            codeStructure: PossibleWordCodes): (List[String], AsortingCriteria) = {
    //result length is achieved and and the function should terminate
    if (reslist.length == codeStructure.code) {
      return (reslist, codeStructure)
    }

    //length is not achieved but there is no more source data
    if (inp.isEmpty || inp.head.size == 0) {
      //fill up with z
      val zFillUpList = ("z"*(codeStructure.code - reslist.size)).split("").toList
      return splitCodeListHelperSingleChar(reslist ++ zFillUpList, List(),codeStructure: PossibleWordCodes)
    }

    //one element missing from res and one or two characters left
    val reslistMissingOne = reslist.size == codeStructure.code - 1
    if (reslistMissingOne && inp.head.size < 3) {
      return splitCodeListHelperSingleChar(reslist ++ List(inp.head), inp.drop(1),codeStructure: PossibleWordCodes)
    }
    //one element missing from res and more than two character left
    if (reslistMissingOne && inp.head.size > 2 && !(codeStructure == PossibleWordCodes.FirstCode)) {
      return splitCodeListHelperSingleChar(reslist ++ List(inp.head.takeRight(2)), inp.drop(1),codeStructure: PossibleWordCodes)
    }
    if (reslistMissingOne && inp.head.size > 2 && (codeStructure == PossibleWordCodes.FirstCode)) {
      return splitCodeListHelperSingleChar(reslist ++ List(inp.head.take(2)), inp.drop(1),codeStructure: PossibleWordCodes)
    }
    
    //input source data length is greater than 1,
    //meaning there are element that must be handled
    if (inp.size > 1) {
      return splitCodeListHelperSingleChar(reslist ++ List(inp.head), inp.drop(1),codeStructure: PossibleWordCodes)
    }
    
    //more than one element missing and 1 or 2 characters left
    if (inp.head.size < 3) {
      return splitCodeListHelperSingleChar(reslist ++ List(inp.head), inp.drop(1),codeStructure: PossibleWordCodes)
    }
    
    //base case: more than one element missing from result, 
    //and source data is grater than 2
    if (inp.head.size > 2) {
      val headOfString = inp.head.take(2)
      val remain = inp.head.drop(2)
      return splitCodeListHelperSingleChar(reslist ++ List(headOfString), List(remain),codeStructure: PossibleWordCodes)
    }
    throw RuntimeException("Unknow termination of " + "splitCodeListHelper")
  }

}
