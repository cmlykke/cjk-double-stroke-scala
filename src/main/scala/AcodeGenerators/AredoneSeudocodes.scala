package AcodeGenerators

import AcodeGenerators.AgenerateFinalSeudoCodes.getNoFillHelper
import AcodeGenerators.AgenerateSeudoCodes.splitCodeListHelperSingleChar
import Adatasources.ManualData.AcodelengthRules
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.{AconwayColl, Aelementstype, Agrapheme, AidsRecur, AsortingCriteria, PossibleWordCodes, SortingCodes}

import scala.collection.immutable.HashMap

object AredoneSeudocodes {

  def seudoSixCodesFromSingleChar(graph: Agrapheme,
                                  conwaymap: HashMap[Agrapheme, AconwayColl]
                                 ):
  Set[(List[String], AsortingCriteria)] = {
    val conwayRes: Option[AconwayColl] = conwaymap.get(graph)
    if (conwayRes.isEmpty) {
      return Set((List(), PossibleWordCodes.FirstFirstFirstFirstFirstLastCode))
    }
    val res: Set[(List[String], AsortingCriteria)] = splitCodeListSingleChar(conwayRes.get.rawConway.rawConway, PossibleWordCodes.FirstFirstFirstFirstFirstLastCode)
    return res  
  }
  
  def getfourCodeSeudoCodes(graph: Agrapheme,
                            elemAndRemainder: Set[List[String]],
                            codeStructure: PossibleWordCodes,
                            codeStructureOriginal: PossibleWordCodes): Set[(List[String], AsortingCriteria)] = {
    var result: Set[(List[String], AsortingCriteria)] = Set()
    if (!elemAndRemainder.head.head.isEmpty && elemAndRemainder.head.head == "飲") {
      val tesmp = ""
    }
    for (eachPair <- elemAndRemainder) {
      var getSplitCodes: Set[(List[String], AsortingCriteria)] = Set()
      if (eachPair.size == 1) {
        getSplitCodes = splitCodeListSingleChar(eachPair, codeStructure)
      }else if (eachPair.size == 2 && PossibleWordCodes.FirstCode == codeStructure) {
         getSplitCodes = Set((List(eachPair.head),codeStructure))
      }else if (eachPair.size == 2 && PossibleWordCodes.FirstLastCode == codeStructure) {
        val temp: Set[(List[String], AsortingCriteria)] = splitCodeListSingleChar(eachPair.tail, PossibleWordCodes.LastCode)
        if (!temp.isEmpty) {
          getSplitCodes = temp.map(x => (List(eachPair.head) ++ x._1, x._2)).toSet
        }else {
          getSplitCodes = Set((List(eachPair.head), codeStructureOriginal))
        }
      }else if (eachPair.size == 2 && PossibleWordCodes.FirstSecondLastCode == codeStructure) {
        val temp: Set[(List[String], AsortingCriteria)] = splitCodeListSingleChar(eachPair.tail, PossibleWordCodes.FirstLastCode)
        if (!temp.isEmpty) {
          getSplitCodes = temp.map(x => (List(eachPair.head) ++ x._1, x._2)).toSet
        } else {
          getSplitCodes = Set((List(eachPair.head), codeStructureOriginal))
        }
      }else if (eachPair.size == 2 && PossibleWordCodes.FirstFirstFirstLastCode == codeStructure) {
        val temp : Set[(List[String], AsortingCriteria)] = splitCodeListSingleChar(eachPair.tail, PossibleWordCodes.FirstSecondLastCode)
        if (!temp.isEmpty) {
          getSplitCodes = temp.map(x => (List(eachPair.head) ++ x._1, x._2)).toSet
        } else {
          getSplitCodes = Set((List(eachPair.head), codeStructureOriginal))
        }
      }else {
        throw new RuntimeException("unexpected combination of elements, remainder and PossibleWordCodes")
      }
      result = result ++ getSplitCodes
    }
    return result.map(x => (x._1, codeStructureOriginal)).toSet
  }

  def splitCodeListSingleChar(inp: List[String],
                              codeStructure: PossibleWordCodes):  Set[(List[String], AsortingCriteria)]  = {
    val helperResults: Set[List[String]] = inp.map(x => splitCodeListSingleCharHelperRedune(Set(), x, x, codeStructure)).flatten.toSet
    val result: Set[(List[String], AsortingCriteria)] = helperResults.map(x => (x, codeStructure)).toSet
    return result
  }

  def splitCodeListSingleCharHelperRedune(result: Set[List[String]],
                                          inp: String,
                                          inpOriginal: String,
                                          codeStructure: PossibleWordCodes):  Set[List[String]] = {
    var nextPair: Set[(String, String)] = Set()
    var newCodeStructure: PossibleWordCodes = codeStructure
    var updatedResult: Set[List[String]] = Set()
    if (inp.size == 0) {
      return result
    }
    if (PossibleWordCodes.FirstFirstFirstFirstFirstLastCode == codeStructure) {
      nextPair = ArollOutConway.getHeadCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.FirstFirstFirstFirstLastCode
    }
    if (PossibleWordCodes.FirstFirstFirstFirstLastCode == codeStructure) {
      nextPair = ArollOutConway.getHeadCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.FirstFirstFirstLastCode
    }
    if (PossibleWordCodes.FirstFirstFirstLastCode == codeStructure) {
      nextPair = ArollOutConway.getHeadCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.FirstSecondLastCode
    }
    if (PossibleWordCodes.FirstSecondLastCode == codeStructure) {
      nextPair = ArollOutConway.getHeadCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.FirstLastCode
    }
    if (PossibleWordCodes.FirstLastCode == codeStructure) {
      nextPair = ArollOutConway.getHeadCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.LastCode
    }
    if (PossibleWordCodes.LastCode == codeStructure) {
      nextPair = ArollOutConway.getTailCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.NoCodes
    }
    if (PossibleWordCodes.FirstCode == codeStructure) {
      nextPair = ArollOutConway.getHeadCodeFromRawConway(inp)
      newCodeStructure = PossibleWordCodes.NoCodes
    }
    if (PossibleWordCodes.NoCodes == codeStructure) {
      return result
    }

    if (result.isEmpty) {
      return nextPair.map(eachTupple => splitCodeListSingleCharHelperRedune(Set(List(eachTupple._1)), eachTupple._2, inpOriginal, newCodeStructure)).flatten.toSet
    }else {
      return nextPair
        .map(eachNewPairTupple => //(newStrPair, newRemainder)
          result.map(eachResultList => //Set(ListOfPreviuslyFoundStrPair)
            splitCodeListSingleCharHelperRedune(Set(eachResultList ++ List(eachNewPairTupple._1)), eachNewPairTupple._2, inpOriginal, newCodeStructure)).flatten).flatten
    }


  }

  def getElementAndRemainder(graph: Agrapheme,
                             conwaymap: HashMap[Agrapheme, AconwayColl],
                             idsmap: HashMap[Agrapheme, String],
                             idsToStrokeMap: Map[String, Aelementstype]):
  Set[List[String]] = {
    if (graph.char == "竹") {
      val test = ""
    }
    var result: Set[List[String]] = Set()
    val originalConway: Option[AconwayColl] = conwaymap.get(graph)
    if (originalConway.isEmpty) {
      throw new RuntimeException("AidsRecur conway not found")
    }
    val localrecur: AidsRecur = AidsRecur(graph, idsmap, conwaymap, originalConway.get.rawConway.rawConway)
    val backslashCleaned: List[String] = originalConway.get.rawConway.rawConway.map(x =>  AgraphemeToStrokeSet.unrollBackSlash(x))
    val elemsfound = AidsRecur.findElementRedune(graph, idsmap, idsToStrokeMap, backslashCleaned, localrecur)
    if (elemsfound.isDefined) {
      val elemCodes: Set[String] = elemsfound.get.strokes
      val remainder: Set[String] = ArollOutConway.getRemainderFromMainAndElem(backslashCleaned,elemCodes.toList)
      if (remainder.size == 0 && elemsfound.get.ids != graph.char && elemsfound.get.unifiedElemet != graph.char ) {
        throw new RuntimeException("elem found through ids, but no element found through conway")
      }else if (remainder.size == 0) {
        result = Set(List(elemsfound.get.unifiedElemet))
      }else {
        result = remainder.map(x => List(elemsfound.get.unifiedElemet, x)).toSet
      }
    } else {
      result = backslashCleaned.map(x => List(x)).toSet
    }
    return result
  }


  def getNoFillCodesFromFourCode(input: Set[(List[String], AsortingCriteria)]):
  Set[(List[String], AsortingCriteria)] = {
    val res = input.map(x => getNoFillHelper(x))
    return res
  }


  private def getNoFillHelper(input: (List[String], AsortingCriteria)): (List[String], AsortingCriteria) = {
    if (input._1.length == 0) {
      throw new RuntimeException("no code can be of length 0")
    }
    if (input._1.head == AcodelengthRules.fill) {
      return input
    }
    if (input._1.last != AcodelengthRules.fill) {
      if (input._1.length == 1) {
        return (input._1, SortingCodes.OneCode)
      } else if (input._1.length == 2) {
        return (input._1, SortingCodes.TwoCode)
      } else if (input._1.length == 3) {
        return (input._1, SortingCodes.ThreeCodeSingleChar)
      } else if (input._1.length == 4) {
        return input
      } else {
        throw new RuntimeException("No Fill codeLength length cant be greater than FourCode")
      }
    }
    return getNoFillHelper((input._1.init, input._2))
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
