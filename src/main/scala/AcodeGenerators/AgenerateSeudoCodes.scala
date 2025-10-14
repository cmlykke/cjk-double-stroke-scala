package AcodeGenerators

import Adatasources.ManualData.AcodelengthRules
import Atypes.{AsortingCriteria, SortingCodes}

object AgenerateSeudoCodes {

  val fil: String = AcodelengthRules.fil

  def convertElemAndRemainderToSeudo(input: Set[(List[String],Int)]):
                                    Set[(List[String], AsortingCriteria)] = {
    val fourAndSixCodes: Set[(List[String], AsortingCriteria)] = input.map(x => splitCodeList(x))
    val noFillCodes: Set[(List[String], AsortingCriteria)] = generateNoFillFromFourCode(fourAndSixCodes)
    return fourAndSixCodes ++ noFillCodes
  }
  
  private def generateNoFillFromFourCode(input: Set[(List[String], AsortingCriteria)]):
                                   Set[(List[String], AsortingCriteria)] = {
    input.filter(x => x._2 == SortingCodes.FourCode).map(x => removeZFillFromFourCode(x._1)).toSet
  }

  private def removeZFillFromFourCode(codelist: List[String]): (List[String], AsortingCriteria) = {
    if (codelist.size == 0) {
      throw new RuntimeException("removeZFill should not result in a zero length code list")
    }
    if (codelist.last != fil) {
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

  private def splitCodeList(inp: (List[String],Int)): (List[String], AsortingCriteria) = {
    splitCodeListHelper(List(), inp._1, inp._2)
  }

  private def splitCodeListHelper(reslist: List[String],
                                  inp: List[String], size: Int): (List[String], AsortingCriteria) = {
    //result length is achieved and and the function should terminate
    if (reslist.length == size) {
      if (size == 4) {
        return (reslist, SortingCodes.FourCode)
      } else if (size == 6) {
        return (reslist, SortingCodes.SixCode)
      } else {
        throw RuntimeException("Unhandled size argument: " + size )
      }
    }
    //length is not achieved but there is no more source data
    if (inp.isEmpty || inp.head.size == 0) {
      //fill up with z
      val zFillUpList = ("z"*(size - reslist.size)).split("").toList
      return splitCodeListHelper(reslist ++ zFillUpList, List(), size)
    }

    //one element missing from res and one or two characters left
    val reslistMissingOne = reslist.size == size - 1
    if (reslistMissingOne && inp.head.size < 3) {
      return splitCodeListHelper(reslist ++ List(inp.head), inp.drop(1), size)
    }
    //one element missing from res and more than two character left
    if (reslistMissingOne && inp.head.size > 2) {
      return splitCodeListHelper(reslist ++ List(inp.head.takeRight(2)), inp.drop(1), size)
    }
    
    //input source data length is greater than 1,
    //meaning there are element that must be handled
    if (inp.size > 1) {
      return splitCodeListHelper(reslist ++ List(inp.head), inp.drop(1), size)
    }
    
    //more than one element missing and 1 or 2 characters left
    if (inp.head.size < 3) {
      return splitCodeListHelper(reslist ++ List(inp.head), inp.drop(1), size)
    }
    
    //base case: more than one element missing from result, 
    //and source data is grater than 2
    if (inp.head.size > 2) {
      val headOfString = inp.head.take(2)
      val remain = inp.head.drop(2)
      return splitCodeListHelper(reslist ++ List(headOfString), List(remain), size)
    }
    throw RuntimeException("Unknow termination of " + "splitCodeListHelper")
  }

}
