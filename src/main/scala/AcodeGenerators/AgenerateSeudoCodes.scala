package AcodeGenerators

object AgenerateSeudoCodes {

  def convertElemAndRemainderToSeudo(input: Set[(List[String],Int)]): Set[List[String]] = {
    val res = input.map(x => splitCodeList(x))
    return res
  }

  def splitCodeList(inp: (List[String],Int)): List[String] = {
    splitCodeListHelper(List(), inp._1, inp._2)
  }

  private def splitCodeListHelper(reslist: List[String], inp: List[String], size: Int): List[String] = {
    //result length is achieved and and the function should terminate
    if (reslist.length == size) { return reslist }
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
