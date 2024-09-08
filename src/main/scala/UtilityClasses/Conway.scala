package UtilityClasses

import staticFileGenerators.Conway.GenerateConwayCodes

class Conway(input: String) {
  val rawConway: String = input
  var relevantConvay: Set[ConwayUnambigous] = Set()
/*
  def getSplitConwayListWithInitial(initial: String,
                                    inputSize: InputSizes): Set[ConwayUnambigous] = {
    val split: Set[String] = Conway.generateConway.expandAlternatives(rawConway)
    val splitsingle: Set[List[String]] = split.map(x => Conway.generateConway.splitSingle(x))
    val withInputSize: Set[List[String]] = splitsingle.flatten(x => Conway.generateConway.adaptToInputSize(x, inputSize))
    //create a test that verify that no second to last elemen has only one line
    //val rightLength: Boolean = checkAllLists(withInputSize)
    //if (!rightLength) {
    //  val tes = ""
    //  throw new Exception("not right length of split conway")
    //}

    val withInputSizeAndInitial: Set[List[String]] = withInputSize.map(locallist => initial :: locallist)


    val res: Set[ConwayUnambigous] = withInputSizeAndInitial.map(x => ConwayUnambigous(x)).toSet
    return res
  }*/

  def getSplitConwayList(inputSize: InputSizes): Set[ConwayUnambigous] = {
    val split: Set[String] = Conway.generateConway.expandAlternatives(rawConway)
    val splitsingle: Set[List[String]] = split.map(x => Conway.generateConway.splitSingle(x))
    val withInputSize: Set[List[String]] = splitsingle.flatten(x => Conway.generateConway.adaptToInputSize(x, inputSize))
    //create a test that verify that no second to last elemen has only one line
    val rightLength: Boolean = checkAllLists(withInputSize)
    if (!rightLength) {
      val tes = ""
      throw new Exception("not right length of split conway")
    }
    //val res: Set[ConwayUnambigous] = withInputSize.map(x => ConwayUnambigous(x)).toSet
    val res: Set[ConwayUnambigous] = {
      // Separate elements based on `is4Code` condition
      val withTrueIs4Code = withInputSize.filter(_.length <= 4).map(new ConwayUnambigous(_, is4Code = true))
      val withFalseIs4Code = withInputSize.filter(_.length > 4).map(new ConwayUnambigous(_, is4Code = false))
      // Combine the sets with priority given to `is4Code = true`
      withFalseIs4Code ++ withTrueIs4Code
    }
    return res
  }



  private def checkAllLists(set: Set[List[String]]): Boolean = set.forall(checkLength)


  private def checkLength(list: List[String]): Boolean = {
    if (list.init.forall(_.length == 2)) true
    else false
  }

  
}

object Conway {
  val generateConway = new GenerateConwayCodes() // Singleton instance
}
